use super::parser::{Node, Pattern, Literal, Op, DataDecl, Module, Decl, Defn};
use std::iter;
use std::collections::HashSet;
use std::hash::Hash;

// use petgraph::graph::Graph;

#[derive(Debug, Clone)]
pub enum NodeDS {
    Lit(i64),
    Var(Arg),
    Abs(Option<Arg>, Box<NodeDS>),
    App(Box<NodeDS>, Box<NodeDS>),
    Let(String, Box<NodeDS>, Box<NodeDS>),
    Match(Box<NodeDS>, Vec<(Pat, NodeDS)>),
}

#[derive(Debug, Clone)]
pub enum Pat {
    Prim(Option<Arg>),
    Lit(i64),
    Cons(String, Vec<Option<Arg>>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Arg {
    Ident(String),
    Internal(u64),
}

#[derive(Debug)]
pub struct ModDS {
    name: String,
    exports: Option<Vec<String>>,
    types: Vec<DataDecl>,
    defns: Vec<(String, NodeDS)>,
}

fn op_to_func(o: Op) -> Arg {
    Arg::Ident(String::from(match o {
        Op::Add => "+",
        Op::Sub => "-",
        Op::Mul => "*",
        Op::Div => "/",
        Op::Equal => "==",
    }))
}

struct Counter(u64);
impl Counter {
    fn next(&mut self) -> Arg {
        let i = self.0;
        self.0 += 1;
        Arg::Internal(i)
    }
}

fn tuple_fn(len: usize) -> String {
    iter::repeat(',').take(len - 1).collect()
}

pub fn desugar(n: Node) -> NodeDS {
    desugar_priv(n, &mut Counter(0))
}

impl ModDS {
    pub fn new(m: Module) -> ModDS {
        let Module{name, exports, decls} = m;
        let mut types = Vec::new();
        let mut defns = Vec::new();
        for d in decls {
            match d {
                Decl::Defn(Defn{name, args, val}) => {
                    let temp = Node::Let(vec![(Pattern::Constructor(String::from("a"), args), val)], Box::new(Node::Lit(Literal::Int(0))));
                    if let NodeDS::Let(_, body, _) = desugar(temp) {
                        defns.push((name, *body));
                    } else {
                        unreachable!();
                    }
                }
                Decl::Type(_, _) => {}, // TODO
                Decl::Data(d) => types.push(d),
            };
        }
        ModDS{name, exports, types, defns}
    }
}

fn desugar_priv(n: Node, next: &mut Counter) -> NodeDS {
    match n {
        Node::Lit(Literal::Int(l)) => NodeDS::Lit(l),
        Node::Lit(Literal::True) => NodeDS::Var(Arg::Ident(String::from("True"))),
        Node::Lit(Literal::False) => NodeDS::Var(Arg::Ident(String::from("False"))),
        Node::Lit(Literal::Unit) => NodeDS::Var(Arg::Ident(String::from("()"))),
        Node::Op(o) => NodeDS::Var(op_to_func(o)),
        Node::FunCall(f, a) => {
            NodeDS::App(
                Box::new(desugar_priv(*f, next)),
                Box::new(desugar_priv(*a, next)),
            )
        }
        Node::Ident(i) => NodeDS::Var(Arg::Ident(i)),
        Node::Let(binds, body) => {
            let n = desugar_priv(*body, next);
            // let binds = binds.into_iter().rev().flat_map()
            binds.into_iter().rev().fold(n, |body, (p, b)| {
                let p = if let Pattern::Tuple(args) = p {
                    Pattern::Constructor(tuple_fn(args.len()), args)
                } else {
                    p
                };
                match p {
                    Pattern::Ident(i) => {
                        NodeDS::Let(i, Box::new(desugar_priv(b, next)), Box::new(body))
                    }
                    Pattern::Constructor(i, args) => {
                        if i.starts_with(char::is_lowercase) {
                            // eta reduction.
                            let inner = args.into_iter().rev().fold(
                                desugar_priv(b, next),
                                |body, arg| match arg {
                                    Pattern::Ident(i) => {
                                        NodeDS::Abs(Some(Arg::Ident(i)), Box::new(body))
                                    }
                                    Pattern::Wild => NodeDS::Abs(None, Box::new(body)),
                                    p => {
                                        let a = next.next();
                                        NodeDS::Abs(
                                            Some(a.clone()),
                                            Box::new(NodeDS::Match(
                                                Box::new(NodeDS::Var(a)),
                                                vec![simplify_arm(p, body, next)],
                                            )),
                                        )
                                    }
                                },
                            );
                            NodeDS::Let(i, Box::new(inner), Box::new(body))
                        } else {
                            NodeDS::Match(
                                Box::new(desugar_priv(b, next)),
                                vec![simplify_arm(Pattern::Constructor(i, args), body, next)],
                            )
                        }
                    }
                    Pattern::Wild => panic!("Can't assign to a wild card."),
                    Pattern::Lit(_) => panic!("Can't assign to a literal."),
                    Pattern::Tuple(_) => unreachable!(),
                }
            })
        }
        Node::Lambda(p, _, b) => {
            match p {
                Pattern::Ident(i) => {
                    NodeDS::Abs(Some(Arg::Ident(i)), Box::new(desugar_priv(*b, next)))
                }
                /*Pattern::Lit(l) => {
					let a = next.next();
					NodeDS::Abs(Some(a.clone()), Box::new(NodeDS::Match(
						Box::new(call("==", NodeDS::Var(a), NodeDS::Lit(l))),
						vec![(Pat::Cons("True", vec![]), desugar_priv(*b))])))
				}*/
                Pattern::Wild => NodeDS::Abs(None, Box::new(desugar_priv(*b, next))),
                p => {
                    let a = next.next();
                    NodeDS::Abs(
                        Some(a.clone()),
                        Box::new(NodeDS::Match(
                            Box::new(NodeDS::Var(a)),
                            vec![simplify_arm(p, desugar_priv(*b, next), next)],
                        )),
                    )
                }
            }
        }
        Node::Match(arg, arms) => {
            NodeDS::Match(
                Box::new(desugar_priv(*arg, next)),
                arms.into_iter()
                    .map(|(p, b)| simplify_arm(p, desugar_priv(b, next), next))
                    .collect(),
            )
        }
        Node::Tuple(args) => {
            let tuple = tuple_fn(args.len());
            args.into_iter().fold(
                NodeDS::Var(Arg::Ident(tuple)),
                |f, e| {
                    NodeDS::App(Box::new(f), Box::new(desugar_priv(e, next)))
                },
            )
        }
        Node::If(cond, t, f) => {
            NodeDS::Match(
                Box::new(desugar_priv(*cond, next)),
                vec![
                    (
                        Pat::Cons(String::from("True"), vec![]),
                        desugar_priv(*t, next)
                    ),
                    (
                        Pat::Cons(String::from("False"), vec![]),
                        desugar_priv(*f, next)
                    ),
                ],
            )
        }
    }
}

fn call(f: String, e1: NodeDS, e2: NodeDS) -> NodeDS {
    // Prioitise literals to facilitate compile time evaluation
    let (e1, e2) = match (e1, e2) {
        (e1 @ NodeDS::Lit(_), e2) |
        (e2, e1 @ NodeDS::Lit(_)) => (e1, e2),
        e => e,
    };
    NodeDS::App(
        Box::new(NodeDS::App(
            Box::new(NodeDS::Var(Arg::Ident(f))),
            Box::new(e1),
        )),
        Box::new(e2),
    )
}
// fn simplify_pat(p: Pattern) -> Pat {
// match p {
// Pattern::Wild => Pat::Prim(None),
// Pattern::Ident(i) => Pat::Prim(Some(Arg::Ident(i))),
// Pattern::Lit(Literal::Unit) => Pat::Cons("()", Vec::new()),
// Pattern::Lit(Literal::True) => Pat::Cons("True", Vec::new()),
// Pattern::Lit(Literal::False) => Pat::Cons("False", Vec::new()),
// Pattern::Lit(Literal::Int(i)) => Pat::Lit(i),
// Pattern::Constuctor(c, args) => Pat::Cons(c, args.into_iter().map(simplify_pat)),
// Pattern::Tuple(ps) => Pat::Cons(tuple_fn, ps.into_iter().map(simplify_pat)),
// }
// }
//
fn simplify_arm(p: Pattern, n: NodeDS, next: &mut Counter) -> (Pat, NodeDS) {
    match p {
        Pattern::Ident(i) => (Pat::Prim(Some(Arg::Ident(i))), n),
        Pattern::Wild => (Pat::Prim(None), n),
        Pattern::Lit(Literal::Unit) => (Pat::Cons(String::from("()"), Vec::new()), n),
        Pattern::Lit(Literal::True) => (Pat::Cons(String::from("True"), Vec::new()), n),
        Pattern::Lit(Literal::False) => (Pat::Cons(String::from("False"), Vec::new()), n),
        Pattern::Lit(Literal::Int(i)) => (Pat::Lit(i), n),
        Pattern::Constructor(ref c, ref args)
            if c.starts_with(char::is_lowercase) && args.len() == 0 => {
            (Pat::Prim(Some(Arg::Ident(c.clone()))), n)
        }
        Pattern::Constructor(c, args) => {
            let mut v = Vec::new();
            let new_args = args.clone()
                .into_iter()
                .enumerate()
                .map(|(i, a)| match a {
                    Pattern::Wild => None,
                    Pattern::Ident(i) => Some(Arg::Ident(i)),
                    Pattern::Constructor(ref i, ref a)
                        if a.len() == 0 && i.starts_with(char::is_lowercase) => {
                        Some(Arg::Ident(i.clone()))
                    }
                    p => {
                        let n = next.next();
                        v.push((i, n.clone()));
                        Some(n)
                    }
                })
                .collect();
            (
                Pat::Cons(c, new_args),
                v.into_iter().rev().fold(n, |n, (i, v)| {
                    let (p, new_n) = simplify_arm(args[i].clone(), n, next);
                    NodeDS::Match(Box::new(NodeDS::Var(v)), vec![(p, new_n)])

                }),
            )
        }
        Pattern::Tuple(args) => {
            simplify_arm(Pattern::Constructor(tuple_fn(args.len()), args), n, next)
        }
    }
}
fn bound_vars<'a>(p: &'a Pattern) -> HashSet<&'a str> {
    match *p {
        Pattern::Ident(ref i) => iter::once(i.as_str()).collect(),
        Pattern::Wild | Pattern::Lit(_) => HashSet::new(),
        Pattern::Constructor(ref i, ref args) if i.starts_with(char::is_lowercase) => {
            iter::once(i.as_str()).collect()
        }
        Pattern::Constructor(_, ref args) |
        Pattern::Tuple(ref args) => args.iter().flat_map(bound_vars).collect(),
    }
}

impl Pat {
    pub fn bound_vars<'a>(&'a self) -> HashSet<&'a Arg> {
        match *self {
            Pat::Prim(Some(ref i)) => iter::once(i).collect(),
            Pat::Cons(_, ref args) => args.iter().flat_map(Option::iter).collect(),
            _ => HashSet::new(),
        }
    }
}

fn without<'a, T: Hash + Eq>(s: HashSet<&'a T>, t: T) -> HashSet<&'a T> {
    let mut s = s;
    s.remove(&t);
    s
}

impl NodeDS {
    pub fn free_vars<'a>(&'a self) -> HashSet<&'a Arg> {
        match *self {
            NodeDS::Lit(_) => HashSet::new(),
            NodeDS::Var(ref i) => iter::once(i).collect(),
            NodeDS::Abs(ref i, ref e) => &e.free_vars() - &i.iter().collect(),
            NodeDS::App(ref f, ref p) => &f.free_vars() | &p.free_vars(),
            NodeDS::Let(ref i, ref b, ref r) => {
                &b.free_vars() | &without(r.free_vars(), Arg::Ident(i.clone()))
            }
            NodeDS::Match(ref arg, ref arms) => {
                &arg.free_vars() |
                    &(arms.iter().flat_map(|&(ref p, ref a)| {
                        &a.free_vars() - &p.bound_vars()
                    })).collect()
            }
        }
    }
}
// fn merge_match(arg: NodeDS, e1: Vec<(Pat, NodeDS)>, e2: Vec<(Pat, NodeDS)>) -> Option<NodeDS> {
// e1.append(e2);
// Some(NodeDS::Match(Box::new(arg), e1))
// }
//
// fn find_bind<'a>(i: &str, n: &'a NodeDS) -> Option<&'a NodeDS> {
// match n {
// l @ &NodeDS::Let(ref b, _, _) if b == i => Some(l),
// &NodeDS::Let(_, _, ref b) => find_bind(i, b),
// _ => None,
// }
// }
//
// fn topo_sort(arms: Vec<Pat, NodeDS>) -> Vec<Pat, NodeDS> {
// let bound = arms.iter().map(|(p, n)| (p.bound_vars(), n.free_vars())).collect();
// let deps = Graph::with_capacity(arms.len(), 0);
// let idxs = bound.into_iter().map(|n| deps.add_node()).collect();
// for f_idx in idxs.iter() {
// let &(_, ref f) = &deps[i];
// for b_idx in idxs.iter() {
// let &(ref b, _) = &deps[j];
// if !f.is_disjoint(b) {
// deps.add_edge(f_idx, b_idx, ())
// }
// }
// }
//
//
// }
//
