use super::parser::{Node, Literal};

use std::fmt::{self, Formatter, Display};

// courtesy of http://dev.stephendiehl.com/fun/index.html
// Write you a Haskell.

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Tr,
    Fl,
    Zero,
    IsZero(Box<Expr>),
    Succ(Box<Expr>),
    Pred(Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
	Bool,
	Nat,
	Func(Box<Type>, Box<Type>),
}

pub struct TypeError(Type, Type);

impl Expr {
	pub fn new(n: Node) -> Result<Expr, &'static str> {
	    match n {
	        Node::Lit(Literal::True) => Ok(Expr::Tr),
	        Node::Lit(Literal::False) => Ok(Expr::Fl),
	        Node::Lit(Literal::Int(0)) => Ok(Expr::Zero),
	        Node::FunCall(f, a) => {
	            match *f {
	                Node::Ident(s) => {
	                    match s.as_str() {
	                        "iszero" => Ok(Expr::IsZero(Box::new(Expr::new(*a)?))),
	                        "succ" => Ok(Expr::Succ(Box::new(Expr::new(*a)?))),
	                        "pred" => Ok(Expr::Pred(Box::new(Expr::new(*a)?))),
	                        _ => Err("Only 'iszero' 'succ' and 'pred' allowed as functions"),
	                    }
	                }
	                _ => Err("Invalid function call"),
	            }
	        }
	        Node::If(c, t, f) => Ok(Expr::If(
	            Box::new(Expr::new(*c)?),
	            Box::new(Expr::new(*t)?),
	            Box::new(Expr::new(*f)?),
	        )),
	        _ => Err("Unsupported expression"),
	    }
	}

    pub fn isNum(&self) -> bool {
        match self {
            &Expr::Zero => true,
            &Expr::Succ(ref t) => t.isNum(),
            _ => false,
        }
    }

    pub fn isVal(&self) -> bool {
        match self {
            &Expr::Tr | &Expr::Fl => true,
            ref t => t.isNum(),
        }
    }

    fn eval_(self) -> Result<Expr, Expr> {
        use Expr::*;
        match self.clone() {
            IsZero(box Zero) => Ok(Tr),
            IsZero(box Succ(box ref t)) if t.isNum() => Ok(Fl),
            /*IsZero(s) => match *s {
				Zero => Some(Tr),
				Succ(t) => if t.isNum() { Some(Fl) } else { Err("Invalid.") }
			}*/
            IsZero(box t) => t.eval_().map(|t| IsZero(Box::new(t))).map_err(|_| self),
            Pred(box Zero) => Ok(Zero),
            Pred(box Succ(box ref t)) if t.isNum() => Ok(t.clone()), // workaround for not being able to move from pattern with gaurd.
            Pred(box t) => t.eval_().map(|t| Pred(Box::new(t))).map_err(|_| self),
            If(box Tr, box c, _) => Ok(c),
            If(box Fl, _, box a) => Ok(a),
            If(box t, c, a) => t.eval_().map(|t| If(Box::new(t), c, a)).map_err(|_| self),
            e => Err(e),
        }
    }

    pub fn eval(self) -> Option<Expr> {
    	let v = nf(self, Expr::eval_);
    	match v.isVal() {
    		true => Some(v),
	    	false => None,
    	}
    }

    pub fn type_of(&self) -> Result<Type, TypeError> {
    	match *self {
    		Expr::Succ(ref e) => {
    			match e.type_of()? {
    				Type::Nat => Ok(Type::Nat),
    				t => Err(TypeError(Type::Nat, t)),
    			}
    		}
    		Expr::Pred(ref e) => {
    			match e.type_of()? {
    				Type::Nat => Ok(Type::Nat),
    				t => Err(TypeError(Type::Nat, t)),
    			}
    		}
    		Expr::IsZero(ref e) => {
    			match e.type_of()? {
    				Type::Nat => Ok(Type::Bool),
    				t => Err(TypeError(Type::Nat, t)),
    			}
    		}
    		Expr::Zero => Ok(Type::Nat),
    		Expr::Tr => Ok(Type::Bool),
    		Expr::Fl => Ok(Type::Bool),
    		Expr::If(ref c, ref t, ref f) => match (c.type_of()?, t.type_of()?, f.type_of()?) {
    			(Type::Bool, t, f) => match t == f {
    				true => Ok(t),
    				false => Err(TypeError(t, f)),
    			},
    			(t, _, _) => Err(TypeError(Type::Bool, t)),
    		},
    	}
    }
}

impl Display for TypeError {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "Got {:?}; expected {:?}.", self.1, self.0)
	}
}

fn nf<T, F: Fn(T) -> Result<T, T>>(v: T, f: F) -> T {
    match f(v) {
        Ok(v) => nf(v, f),
        Err(v) => return v,
    }
}