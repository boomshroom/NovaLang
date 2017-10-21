use std::rc::Rc;

use super::parser::{Node, Op, Literal, Pattern};
use super::types::{self, TypeEnv};

#[derive(Debug,Clone)]
pub enum Value {
	Bool(bool),
	Int(i64),
	Box(Box<Value>),
	Closure(Node, Rc<Env>),
	Builtin(String),
	BinOp(Op, Option<Box<Value>>),
	Tuple(Vec<Value>),
}

#[derive(Debug)]
pub struct Env {
	name: String,
	val: Value,
	next: Option<Rc<Env>>,
}

impl Value {
	fn eval_(n: Node, env: Rc<Env>) -> Value {
		match n {
			Node::Lit(Literal::Int(i)) => Value::Int(i),
			Node::Lit(Literal::True) => Value::Bool(true),
			Node::Lit(Literal::False) => Value::Bool(false),
			Node::Ident(i) => match i.as_str() {
				"iszero" | "succ" | "pred" | "Box" | "unwrap" => Value::Builtin(i.clone()),
				i => env.lookup(i).expect("Undefined variable."),
			},
			Node::Op(o) => Value::BinOp(o, None),
			Node::FunCall(l, p) => {
				let p = Value::eval_(*p, env.clone());
				match Value::eval_(*l, env) {
					Value::Closure(Node::Lambda(Pattern::Ident(a), _, b), e) => Value::eval_(*b, Env::add(e, a, p)),
					Value::BinOp(o, None) => Value::BinOp(o, Some(Box::new(p))),
					Value::BinOp(o, Some(box Value::Int(i))) => match (o, p) {
						(Op::Add, Value::Int(j)) => Value::Int(i+j),
						(Op::Sub, Value::Int(j)) => Value::Int(i-j),
						(Op::Mul, Value::Int(j)) => Value::Int(i*j),
						(Op::Div, Value::Int(j)) => Value::Int(i/j),
						(_, _) => panic!("Expected int for binary operator.")
					},
					Value::Builtin(b) => match (b.as_str(), p) {
						("iszero", Value::Int(i)) => Value::Bool(i == 0),
						("succ", Value::Int(i)) => Value::Int(i + 1),
						("pred", Value::Int(i)) => Value::Int(i - 1),
						("Box", i @ Value::Int(_)) => Value::Box(Box::new(i)),
						("unwrap", Value::Box(i @ box Value::Int(_))) => *i,
						(_, _) => panic!("Unsupported builtin."),
					},
					_ => panic!("Invalid function call."),
				}
			}
			l @ Node::Lambda(_,_,_) => Value::Closure(l, env),
			Node::Let(bs, e) => {
				let env = bs.into_iter().fold(env, |acc, (p, b)| Env::add_pattern(acc.clone(), p, Value::eval_(b, acc)));
				Value::eval_(*e, env)
			}
			n => panic!("Unimplemented Node: {:?}", n),
		}
	}

	pub fn eval(n: Node) -> Result<Value, &'static str> {
		//let env = Env::new();
		types::check(&n, &TypeEnv::new()).map(|_| Value::eval_(n, Rc::new(Env{name: "".into(), val: Value::Builtin("".into()), next: None})))
	}
}

impl Env {
	fn lookup(&self, i: &str) -> Option<Value> {
		match i == self.name.as_str() {
			true => Some(self.val.clone()),
			false => self.next.as_ref().and_then(|e| e.lookup(i)),
		}
	}

	fn add(s: Rc<Env>, i: String, v: Value) -> Rc<Env> {
		Rc::new(Env{name: i, val: v, next: Some(s)})
	}

	fn add_node(s: Rc<Env>, i: String, v: Node) -> Rc<Env> {
		Rc::new(Env{name: i, val: Value::eval_(v, s.clone()), next: Some(s)})
	}

	fn add_pattern(s: Rc<Env>, p: Pattern, v: Value) -> Rc<Env> {
		match (p, v) {
			(Pattern::Ident(i), v) => Env::add(s, i, v),
			(Pattern::Tuple(ps), Value::Tuple(vs)) => ps.into_iter().zip(vs).fold(s, |env, (p, v)| Env::add_pattern(env, p, v)),
			(Pattern::Constructor(cons, p), v) => match (cons.as_str(), v) {
				("Box", Value::Box(box v)) => Env::add_pattern(s, *p, v),
				(_, _) => panic!("Unsupported constructor."),
			},
			(_, _) => panic!("Unsupported pattern."),
		}
	}
}