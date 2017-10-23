use super::parser::{Node, Pattern, Literal, Op};
use std::iter;

#[derive(Debug)]
pub enum NodeDS {
    Lit(i64),
    Var(Arg),
    Abs(Option<Arg>, Box<NodeDS>),
    App(Box<NodeDS>, Box<NodeDS>),
    Let(String, Box<NodeDS>, Box<NodeDS>),
    Match(Box<NodeDS>, Vec<(Pat, NodeDS)>),
}

#[derive(Debug)]
pub enum Pat {
    Prim(Option<Arg>),
    Lit(i64),
    Cons(String, Vec<Option<Arg>>),
}

#[derive(Clone, Debug)]
pub enum Arg {
    Ident(String),
    Internal(u64),
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
    iter::once(',').cycle().take(len - 1).collect()
}

pub fn desugar(n: Node) -> NodeDS {
    desugar_priv(n, &mut Counter(0))
}

fn desugar_priv(n: Node, next: &mut Counter) -> NodeDS {
    match n {
        Node::Lit(Literal::Int(l)) => NodeDS::Lit(l),
        Node::Lit(Literal::True) => NodeDS::Var(Arg::Ident(String::from("True"))),
        Node::Lit(Literal::False) => NodeDS::Var(Arg::Ident(String::from("False"))),
        Node::Lit(Literal::Unit) => NodeDS::Var(Arg::Ident(String::from("()"))),
        Node::Op(o) => NodeDS::Var(op_to_func(o)),
        Node::FunCall(f, a) => {
            NodeDS::App(Box::new(desugar_priv(*f, next)),
                        Box::new(desugar_priv(*a, next)))
        }
        Node::Ident(i) => NodeDS::Var(Arg::Ident(i)),
        Node::Let(binds, body) => {
            let n = desugar_priv(*body, next);
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
                            let inner = args.into_iter()
                                .rev()
                                .fold(desugar_priv(b, next), |body, arg| match arg {
                                    Pattern::Ident(i) => {
                                        NodeDS::Abs(Some(Arg::Ident(i)), Box::new(body))
                                    }
                                    Pattern::Wild => NodeDS::Abs(None, Box::new(body)),
                                    p => {
                                        let a = next.next();
                                        NodeDS::Abs(Some(a.clone()),
                                            Box::new(NodeDS::Match(Box::new(NodeDS::Var(a)),
                                                          vec![simplify_arm(p, body, next)])))
                                    }
                                });
                            NodeDS::Let(i, Box::new(inner), Box::new(body))
                        } else {


                            NodeDS::Match(Box::new(desugar_priv(b, next)),
                                      vec![simplify_arm(Pattern::Constructor(i, args), body, next)])
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
				Pattern::Ident(i) => NodeDS::Abs(Some(Arg::Ident(i)), Box::new(desugar_priv(*b, next))),
				/*Pattern::Lit(l) => {
					let a = next.next();
					NodeDS::Abs(Some(a.clone()), Box::new(NodeDS::Match(
						Box::new(call("==", NodeDS::Var(a), NodeDS::Lit(l))),
						vec![(Pat::Cons("True", vec![]), desugar_priv(*b))])))
				}*/
				Pattern::Wild => NodeDS::Abs(None, Box::new(desugar_priv(*b, next))),
				p => {
					let a = next.next();
					NodeDS::Abs(Some(a.clone()), Box::new(NodeDS::Match(Box::new(NodeDS::Var(a)),
						vec![simplify_arm(p, desugar_priv(*b, next), next)])))
				}
			}
        }
        Node::Match(arg, arms) => {
            NodeDS::Match(Box::new(desugar_priv(*arg, next)),
                          arms.into_iter()
                              .map(|(p, b)| simplify_arm(p, desugar_priv(b, next), next))
                              .collect())
        }
        Node::Tuple(args) => {
            let tuple = tuple_fn(args.len());
            args.into_iter().fold(NodeDS::Var(Arg::Ident(tuple)),
                                  |f, e| NodeDS::App(Box::new(f), Box::new(desugar_priv(e, next))))
        }
        Node::If(cond, t, f) => {
            NodeDS::Match(Box::new(desugar_priv(*cond, next)),
                          vec![(Pat::Cons(String::from("True"), vec![]), desugar_priv(*t, next)),
                               (Pat::Cons(String::from("False"), vec![]), desugar_priv(*f, next))])
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
    NodeDS::App(Box::new(NodeDS::App(Box::new(NodeDS::Var(Arg::Ident(f))), Box::new(e1))),
                Box::new(e2))
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
        Pattern::Constructor(c, args) => {
            let mut v = Vec::new();
            let new_args = args.clone()
                .into_iter()
                .enumerate()
                .map(|(i, a)| match a {
                    Pattern::Wild => None,
                    Pattern::Ident(i) => Some(Arg::Ident(i)),
                    p => {
                        let n = next.next();
                        v.push((i, n.clone()));
                        Some(n)
                    }
                })
                .collect();
            (Pat::Cons(c, new_args),
             v.into_iter().rev().fold(n, |n, (i, v)| {
                let (p, new_n) = simplify_arm(args[i].clone(), n, next);
                NodeDS::Match(Box::new(NodeDS::Var(v)), vec![(p, new_n)])

            }))
        }
        Pattern::Tuple(args) => {
            simplify_arm(Pattern::Constructor(tuple_fn(args.len()), args), n, next)
        }
    }
}
