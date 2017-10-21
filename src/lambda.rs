use super::parser::{self, Node, Pattern, Literal};

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Var(String),
    App(Box<Expr>, Box<Expr>),
    Lam(String, Box<Expr>),
    Lit(Lit),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Lit {
    LInt(i64),
    LBool(bool),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
	Bool,
	Nat,
	Func(Box<Type>, Box<Type>),
	Unknown,
}

pub struct TypeError(Type, Type);

struct TypeEnv<'a> {
    var: &'a str,
    val: &'a Expr,
    next: Link<'a>,
}

type Link<'a> = Option<&'a TypeEnv<'a>>

struct Env<'a> {
	head: Link<'a>,
}

impl Expr {
    pub fn new(n: Node) -> Result<Expr, &'static str> {
        Ok(match n {
            Node::Lambda(Pattern::Ident(i), _, e) => Expr::Lam(i, Box::new(Expr::new(*e)?)),
            Node::FunCall(l, a) => Expr::App(Box::new(Expr::new(*l)?), Box::new(Expr::new(*a)?)),
            Node::Ident(i) => Expr::Var(i),
            Node::Lit(Literal::True) => Expr::Lit(Lit::LBool(true)),
            Node::Lit(Literal::False) => Expr::Lit(Lit::LBool(false)),
            Node::Lit(Literal::Int(i)) => Expr::Lit(Lit::LInt(i)),
            Node::Let(vars, b) => vars.into_iter().rev().fold(Expr::new(*b), |acc, (bind, val)| 
            	match (acc, bind) {
            		(Err(msg), _) => Err(msg),
            		(Ok(acc), Pattern::Ident(var)) => {
		            	Expr::new(val).map(|val| Expr::App(Box::new(Expr::Lam(var, Box::new(acc))), Box::new(val)))
		            }
		            (Ok(acc), _) => Err("Only identifiers allowed in let."),
		        })?,
            //Node::If(c, t, f) => Expr::If(Box::new(Expr::new(*c)?), Box::new(Expr::new(*t)?), Box::new(Expr::new(*f)?)),
            _ => return Err("Invalid expression."),
        })
    }

    fn substitute(self, i: &str, e: &Expr) -> Expr {
        match self {
            Expr::Var(v) => {
                match v.as_str() == i {
                    true => e.clone(),
                    false => Expr::Var(v),
                }
            }
            Expr::Lit(l) => Expr::Lit(l),
            Expr::App(f, a) => {
                Expr::App(Box::new(f.substitute(i, e)), Box::new(a.substitute(i, e)))
            }
            Expr::Lam(l, b) => {
                if l.as_str() == i {
                    Expr::Lam(l, b)
                } else if e.free_var(l.as_str()) {
                    panic!("Free variable")
                } else {
                    Expr::Lam(l.clone(), Box::new(b.clone().substitute(i, e)))
                }
            }
            /*Expr::Lam(ref l, ref b) if l.as_str() != i && !e.free_var(l.as_str()) => {
                Expr::Lam(l.clone(), Box::new(b.clone().substitute(i, e)))
            }*/
            e => e,
        }
    }

    fn beta(self) -> Expr {
        match self {
            Expr::App(box Expr::Lam(ref a, ref b), ref v) => {
                b.clone().beta().substitute(a.as_str(), &v.clone().beta())
            }
            e => e,
        }
    }

    fn reduce_(self) -> Result<Expr, Expr> {
    	match self.clone() {
    		e @ Expr::App(box Expr::Lam(_, _), _) => Ok(e.beta()),
    		Expr::App(f, a) => {
    			match (f.reduce_(), a.reduce_()) {
    				(Ok(f), Ok(a)) | (Ok(f), Err(a)) | (Err(f), Ok(a)) => Ok(Expr::App(Box::new(f), Box::new(a))),
    				(Err(f), Err(a)) => Err(Expr::App(Box::new(f), Box::new(a))),
    			}
    			
    		},
    		Expr::Lam(a, b) => Ok(Expr::Lam(a, Box::new(b.reduce_().map_err(|_| self)?))),
    		e => Err(e),
    	}
    }

    pub fn reduce(self) -> Expr {
    	nf(self, Expr::reduce_)
    }

    fn eta(self) -> Expr {
        match self {
            Expr::Lam(ref a, box Expr::App(box ref e, box Expr::Var(ref v)))
                if a == v && !e.free_var(a.as_str()) => e.clone(),
            e => e,
        }
    }

    fn free_var(&self, v: &str) -> bool {
        match *self {
            Expr::Var(ref i) => i.as_str() == v,
            Expr::Lit(_) => false,
            Expr::App(ref f, ref e) => f.free_var(v) || e.free_var(v),
            Expr::Lam(ref l, ref e) => l.as_str() != v && e.free_var(v),
        }
    }

    pub fn type_of(&self, vars: &HashMap<String, Expr>) -> Result<Type, TypeError> {
    	match *self {
    		Expr::Lit(Lit::LInt(_)) => Ok(Type::Int),
    		Expr::Lit(Lit::LBool(_)) => Ok(Type::Bool),
    		Expr::Lam(a, t, e) => Ok(Type::Func(Box::new(t), Box::new(e.type_of(vars)?))),
    		Expr::App(f, e) => match (f.type_of(vars)?, e.type_of(vars)?) {
    			(Type::Func(a, r), e) match a == e {
    				true => Ok(r),
    				false => Err(TypeError(a, e)),
    			}
    			(t, a) => Err(TypeError(Type::Func(Box::new(a), Box::new(Type::Unknown)), t)),
    		},
    		Expr::If(c, t, f) => match (c.type_of(vars)?, t.type_of(vars)?, f.type_of(vars)?) {
    			(Type::Bool, t, f) => match t == f {
    				true => Ok(t),
    				false => Err(TypeError(t, f)),
    			}
    			(t, _, _) => Err(TypeError(Type::Bool, t)),
    		}
    	}
    }

    fn apply_type(&self, arg: Expr, vars: Env) -> Result<Type, TypeError> {

    }
}

impl Type {
	pub fn new(t: parser::Type) -> Result<Type, &'static str> {
		match t {
			parser::Type::Int => Ok(Type::Nat),
			parser::Type::Bool => Ok(Type::Bool),
			parser::Type::Func(a, e) => Ok(Type::Func(Box::new(Type::new(*a)?), Box::new(Type::new(*e)?))),
			_ => Err("Invalid Type."),
		}
	}
}

fn nf<T, F: Fn(T) -> Result<T, T>>(v: T, f: F) -> T {
    match f(v) {
        Ok(v) => nf(v, f),
        Err(v) => return v,
    }
}