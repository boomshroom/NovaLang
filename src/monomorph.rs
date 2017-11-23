use super::types::{TypedNode, TypedMod, Type, TypeInfo, TId, Types, Scheme, EnumDecl};
use super::desugar::{Arg, Pat};
use std::iter::once;
use std::collections::{HashSet, HashMap};

#[derive(Debug)]
pub enum Node {
    Lit(i64),
    Var(Arg, Type),
    Abs(Option<Arg>, Box<Node>, Type, Vec<(Arg, Type)>),
    App(Box<Node>, Box<Node>),
    Let(String, Box<Node>, Box<Node>),
    Match(Box<Node>, Vec<(Pat, Node)>, Type),
    Constr(Vec<Node>, Type, u64),
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub exports: Option<Vec<String>>,
    pub types: Vec<(String, Scheme<EnumDecl>)>,
    pub defns: Vec<(String, Type, Node)>,
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
        let mut new_defns = Vec::with_capacity(defns.len());

        let mut stack = Vec::new();
        stack.push((
            String::from("main"),
            Type::Func(
                Box::new(Type::Alias(
                    String::from("Tuple"),
                    vec![
                        Type::Int,
                        Type::Ptr(Box::new(Type::Ptr(Box::new(Type::Int8)))),
                    ],
                )),
                Box::new(Type::Int),
            ),
        ));

        let mut info = TypeInfo::new();

        while let Some(item) = stack.pop() {
            if search(new_defns.as_slice(), &item) {
                continue;
            }

            let (name, ty) = item;
            match name.as_str() {
                // Builtin
                "llvm_add_int64" | "llvm_sub_int64" | "llvm_mul_int64" | "llvm_div_int64" => {}
                _ => {
                    let n = defns.get(&name).expect(
                        format!(
                            "Undeclared name: {}\n{:?}",
                            name,
                            defns
                        ).as_str(),
                    );
                    info = info.compose(
                        match n.get_type(&TypeInfo::new()) {
                            Scheme::Type(t) |
                            Scheme::Forall(t, _) => t,
                        }.unify(ty.clone())
                            .unwrap(),
                    );

                    let n = match n {
                        &Scheme::Type(ref n) => n,
                        &Scheme::Forall(ref n, _) => n,
                    };

                    let node = monomorph(n.clone().apply(&info));
                    stack.extend(node.free_vars());
                    new_defns.push((name, ty.apply(&info), node));
                }
            }
        }

        Module {
            name: name,
            exports: exports,
            types: types,
            defns: new_defns,
        }
    }
}

fn search(list: &[(String, Type, Node)], entry: &(String, Type)) -> bool {
    let &(ref i1, ref t1) = entry;
    for &(ref i2, ref t2, _) in list {
        if i1 == i2 && t1 == t2 {
            return true;
        }
    }
    return false;
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
                        let info = e.get_type(&TypeInfo::new()).unify(t).unwrap();
                        Node::Let(
                            i.clone(),
                            Box::new(monomorph(e.clone().apply(&info))),
                            Box::new(b),
                        )
                    })
                }
            }
        }
        TypedNode::Constr(args, t, i) => {
            Node::Constr(args.into_iter().map(monomorph).collect(), t, i)
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
                    .filter(|&&(ref p, _)| {
                        !p.bound_vars().contains(&Arg::Ident(String::from(i)))
                    })
                    .flat_map(|&(_, ref b)| uses(b, i))
                    .collect()
        }
        &TypedNode::Constr(ref args, _, _) => args.iter().flat_map(|a| uses(a, i)).collect(),
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
            Node::App(ref f, _) => f.get_ret_type().expect("Non function in application."),
            Node::Let(_, _, ref b) => b.get_type(),
            Node::Match(_, _, ref b) => b.clone(),
            Node::Constr(_, ref t, _) => t.clone(),
        }
    }

    pub fn get_ret_type(&self) -> Option<Type> {
        match self.get_type() {
            Type::Func(_, b) => Some(*b),
            Type::Closure(_, b, _) => Some(*b),
            t => None,
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
            Node::Constr(ref a, _, _) => a.iter().flat_map(Node::free_vars).collect(),
        }
    }
}
