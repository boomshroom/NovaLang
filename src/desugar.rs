use super::parser::{Node, Pattern, Literal, Op};

enum NodeDS {
	Lit(Literal),
	Var(String),
	Abs(String, Box<NodeDS>),
	App(Box<NodeDS>, Box<NodeDS>),
	Let(String, Box<NodeDS>, Box<NodeDS>),
	Builtin(Builtin),
	
}

#[derive(Debug,Copy,Clone)]
enum Builtin {
	Add,
	Sub,
	Mul,
	Div,
	If,
	Tup(usize),
}

fn op_to_builtin(o: Op) -> Builtin {
	match o {
		Op::Add => Builtin::Add,
		Op::Sub => Builtin::Sub,
		Op::Mul => Builtin::Mul,
		Op::Div => Builtin::Div,
	}
}

pub fn desugar(n: Node) -> NodeDS {
	match n {
		Node::Lit(l) => NodeDS::Lit(l),
		Node::Op(o) => NodeDS::Builtin(op_to_builtin(o)),
		Node::FunCall(f, a) => NodeDS::App(Box::new(desugar(*f)), Box::new(desugar(*a))),
		Node::Ident(i) => NodeDS::Var(i),
		Node::Let(binds, body) => 
	}
}