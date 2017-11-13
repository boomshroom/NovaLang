use super::w_ds::{TypedNode, TypedMod, Type, TId, Types, Scheme, EnumDecl};
use super::desugar::{Arg, Pat};
use std::iter::once;
use std::collections::{HashSet, HashMap, VecDeque};

#[derive(Debug)]
pub enum Node {
    Lit(i64),
    Var(Arg, Type),
    Abs(Option<Arg>, Box<Node>, Type, Vec<(Arg, Type)>),
    App(Box<Node>, Box<Node>),
    Let(String, Box<Node>, Box<Node>),
    Match(Box<Node>, Vec<(Pat, Node)>, Type),
}

#[derive(Debug)]
pub struct Module {
    name: String,
    exports: Option<Vec<String>>,
    types: Vec<(String, Scheme<EnumDecl>)>,
    defns: HashMap<(String, Type), Node>,
}

impl Module {
    pub fn new(m: TypedMod) -> Module {
        let TypedMod {
            name,
            exports,
            types,
            defns,
        } = m;

        let defns = defns.into_iter().collect::<HashMap<_, _>>();
        let mut new_defns = HashMap::with_capacity(defns.len());

        let mut queue = VecDeque::new();
        queue.push_back((
            String::from("main"),
            Type::Func(
                Box::new(Type::Tuple(vec![
                    Type::Int,
                    Type::Ptr(Box::new(Type::Ptr(Box::new(Type::Unit)))),
                ])),
                Box::new(Type::Int),
            ),
        ));

        while let Some(item) = queue.pop_front() {
            if new_defns.contains_key(&item) {
                continue;
            }
            let (name, ty) = item;
            let node = match defns.get(&name).expect("Function not defined.") {
                &Scheme::Type(ref n) => {
                    assert_eq!(n.get_type(), ty);
                    monomorph(n.clone())
                }
                &Scheme::Forall(ref n, _) => {
                    let info = n.get_type().unify(ty.clone()).unwrap();
                    monomorph(n.clone().apply(&info))
                }
            };
            queue.extend(node.free_vars());
            new_defns.insert((name, ty), node);
        }

        Module {
            name,
            exports,
            types,
            defns: new_defns,
        }
    }
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

    pub fn free_vars(&self) -> HashMap<String, Type> {
        match *self {
            Node::Lit(_) => HashMap::new(),
            Node::Var(Arg::Ident(ref i), ref t) => once((i.clone(), t.clone())).collect(),
            Node::Var(Arg::Internal(_), _) => HashMap::new(),
            Node::Abs(ref a, ref b, _, ref c) => {
                let mut free = b.free_vars();
                if let &Some(Arg::Ident(ref a)) = a {
                    free.remove(a);
                }
                for &(ref a, _) in c.iter() {
                    if let &Arg::Ident(ref a) = a {
                        free.remove(a);
                    }
                }
                free
            }
            Node::App(ref f, ref a) => f.free_vars().into_iter().chain(a.free_vars()).collect(),
            Node::Let(ref i, ref e, ref b) => {
                let mut free = e.free_vars();
                free.extend(b.free_vars());
                free.remove(i);
                free
            }
            Node::Match(ref a, ref arms, _) => {
                arms.iter()
                    .flat_map(|&(ref p, ref b)| {
                        let mut free = b.free_vars();
                        for a in p.bound_vars() {
                            if let &Arg::Ident(ref a) = a {
                                free.remove(a);
                            }
                        }
                        free
                    })
                    .chain(a.free_vars())
                    .collect()
            }
        }
    }
}
