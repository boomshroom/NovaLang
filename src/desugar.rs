use super::parser::{Node, Pattern, Literal, Op};
use std::iter;

enum NodeDS {
	Lit(i64),
	Var(Arg),
	Abs(Option<Arg>, Box<NodeDS>),
	App(Box<NodeDS>, Box<NodeDS>),
	Let(String, Box<NodeDS>, Box<NodeDS>),
	Match(Box<NodeDS>, Vec<(Pat,NodeDS)>),
}

enum Pat {
	Prim(Option<Arg>)
	Unit,
	Lit(i64),
	Cons(String, Vec<PrimPat>),
}

enum Arg {
	Ident(String),
	Internal(u64),
}

fn op_to_func(o: Op) -> Arg {
	Arg::Ident(match o {
		Op::Add => "+",
		Op::Sub => "-",
		Op::Mul => "*",
		Op::Div => "/",
	})
}

struct Counter(u64);
impl Counter {
    fn next(&mut self) -> Arg {
    	let i = self.0;
    	self.0 += 1;
    	Arg::Internal(i)
    }
}

pub fn desugar(n: Node, next: &mut Counter) -> NodeDS {
	match n {
		Node::Lit(l) => NodeDS::Lit(l),
		Node::Op(o) => NodeDS::Var(op_to_func(o)),
		Node::FunCall(f, a) => NodeDS::App(Box::new(desugar(*f, next)), Box::new(desugar(*a, next))),
		Node::Ident(i) => NodeDS::Var(Arg::Ident(i)),
		Node::Let(binds, body) => {
			let mut n = desugar(*body, next);
			binds.into_iter().rev().fold(n, |body, (p, b)| match p {
				Pattern::Ident(i) => NodeDS::Let(i, Box::new(desugar(b, next)), Box::new(body)),
				Pattern::Constuctor(i, args) => {
					if i.starts_with(char::is_uppercase) {
						NodeDS::Match(Box::new(b), vec![(simplify_pat(Pattern::Constuctor(i, args)), body)])
					} else {
						let inner =  args.into_iter().rev().fold(b, |body, arg| match arg {
							Pattern::Ident(i) => NodeDS::Abs(Some(Arg::Ident(i)), Box::new(body)),
							Pattern::Wild => NodeDS::Abs(None, Box::new(body)),
							p => {
								let a = next.next();
								NodeDS::Abs(Some(a), NodeDS::Match(Box::new(NodeDS::Var(a)), vec![(simplify_pat(p), body)]))
							}
						});
						NodeDS::Let(i, inner, Box::new(body))
					}
				}
				Pattern::Wild => panic!("Can't assign to a wild card."),
				Pattern::Lit(_) => panic!("Can't assign to a literal."),
			})
		},
		Node::Lambda(p, _, b) => {
			match p {
				Pattern::Ident(i) => NodeDS::Abs(Some(Arg::Ident(i)), Box::new(desugar(b, next))),
				Pattern::Lit(l) => {
					let a = next.next();
					NodeDS::Abs(Some(a), Box::new())
				}
			}
		}
	}
}

fn simplify_pat(p: Pattern) -> Pat {
	match p {
		Pattern::Wild => Pat::Prim(None),
		Pattern::Ident(i) => Pat::Prim(Arg::Ident(i)),
		Pattern::Lit(Literal::Unit) => Pat::Cons("()", Vec::new()),
		Pattern::Lit(Literal::True) => Pat::Cons("True", Vec::new()),
		Pattern::Lit(Literal::False) => Pat::Cons("False", Vec::new()),
		Pattern::Lit(Literal::Int(i)) => Pat::Prim(PrimPat::Lit(i)),
		Pattern::Constuctor(c, args) => Pat::Cons(c, args.into_iter().map(simplify_pat)),
		Pattern::Tuple(ps) => Pat::Cons(iter::once(',').cycle().take(ps.len()-1).collect(), ps.into_iter().map(simplify_pat)),
	}
}