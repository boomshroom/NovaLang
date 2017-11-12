use super::w_ds::{TypedNode, Type, TId, Types, Scheme};
use super::desugar::{Arg, Pat};
use std::iter::once;
use std::collections::HashSet;

#[derive(Debug)]
pub enum Node {
    Lit(i64),
    Var(Arg, Type),
    Abs(Option<Arg>, Box<Node>, Type, Vec<(Arg, Type)>),
    App(Box<Node>, Box<Node>),
    Let(String, Box<Node>, Box<Node>),
    Match(Box<Node>, Vec<(Pat, Node)>, Type),
}

pub fn monomorph(n: TypedNode) -> Node {
    match n {
        TypedNode::Lit(i) => Node::Lit(i),
        TypedNode::Var(a, t) => Node::Var(a, t),
        TypedNode::Abs(a, b, at, c) => Node::Abs(a, Box::new(monomorph(*b)), at, c),
        TypedNode::App(f, a) => Node::App(Box::new(monomorph(*f)), Box::new(monomorph(*a))),
        TypedNode::Match(a, bs, t) => {
            Node::Match(
                Box::new(monomorph(*a)),
                bs.into_iter().map(|(p, n)| (p, monomorph(n))).collect(),
                t,
            )
        }
        TypedNode::Let(i, e, b) => {
            match *e {
                Scheme::Type(e) => Node::Let(i, Box::new(monomorph(e)), Box::new(monomorph(*b))),
                Scheme::Forall(e, _) => {
                    let uses = uses(&*b, i.as_str());
                    uses.into_iter().fold(monomorph(*b), |b, t| {
                        let info = e.get_type().unify(t).unwrap();
                        Node::Let(
                            i.clone(),
                            Box::new(monomorph(e.clone().apply(&info))),
                            Box::new(b),
                        )
                    })
                }
            }
        }
    }
}

fn uses(s: &TypedNode, i: &str) -> HashSet<Type> {
    match s {
        &TypedNode::Lit(_) => HashSet::new(),
        &TypedNode::Var(Arg::Ident(ref i2), ref t) if i == i2 => once(t.clone()).collect(),
        &TypedNode::Var(_, _) => HashSet::new(),
        &TypedNode::Abs(Some(Arg::Ident(ref i2)), _, _, _) if i == i2 => HashSet::new(),
        &TypedNode::Abs(_, ref b, _, _) => uses(b, i),
        &TypedNode::App(ref f, ref a) => &uses(f, i) | &uses(a, i),
        &TypedNode::Let(ref i2, _, _) if i == i2 => HashSet::new(),
        &TypedNode::Let(_, ref e, ref b) => {
            &uses(
                match &**e {
                    &Scheme::Type(ref e) |
                    &Scheme::Forall(ref e, _) => e,
                },
                i,
            ) | &uses(b, i)
        }
        &TypedNode::Match(ref a, ref arms, _) => {
            &uses(a, i) |
                &arms.iter()
                    .flat_map(|&(ref p, ref b)| if p.bound_vars().contains(&Arg::Ident(
                        String::from(i),
                    ))
                    {
                        HashSet::new()
                    } else {
                        uses(b, i)
                    })
                    .collect()
        }
    }
}

impl Node {
    pub fn get_type(&self) -> Type {
        match *self {
            Node::Lit(_) => Type::Int,
            Node::Var(_, ref t) => t.clone(),
            Node::Abs(_, ref b, ref p, ref c) => {
                if c.len() == 0 {
                    Type::Func(Box::new(p.clone()), Box::new(b.get_type()))
                } else {
                    Type::Closure(
                        Box::new(p.clone()),
                        Box::new(b.get_type()),
                        c.iter().map(|&(_, ref t)| t.clone()).collect(),
                    )
                }
            }
            Node::App(ref f, _) => f.get_ret_type().unwrap_or(Type::Free(TId::new())),
            Node::Let(_, _, ref b) => b.get_type(),
            Node::Match(_, _, ref b) => b.clone(),
        }
    }

    pub fn get_ret_type(&self) -> Option<Type> {
        match self.get_type() {
            Type::Func(_, b) => Some(*b),
            Type::Closure(_, b, _) => Some(*b),
            _ => None,
        }
    }
}
