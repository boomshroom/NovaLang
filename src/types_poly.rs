use super::parser::{self, Node, Literal, Pattern, Op};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Kind {
	Star(Type),
	Arrow(TId, Box<Kind>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
	Unit,
    Int,
    Bool,
    Func(Box<Type>, Box<Type>),
    Free(TId),
}

struct TypeEnv<'a> {
	var: &'a str,
	val: Kind,
	tail: Option<&'a TypeEnv<'a>>,
}

fn lit_type(l: &Literal) -> Kind {
	Kind::Star(match l {
		&Literal::Unit => Type::Unit,
		&Literal::Int(_) => Type::Int,
		&Literal::True | &Literal::False => Type::Bool,
	})
}

type TId = u64

impl Kind {
	fn of<'a>(n: &Node, env: &TypeEnv<'a>) -> Result<Kind, &'static str> {
		match n {
        	&Node::Lit(ref v) => Ok(lit_type(v)),
	        //&Node::Ident(ref i) => env.lookup(i).cloned().ok_or("Value undefined."),
	        &Node::Op(o) => {
	            match o {
	                Op::Add | Op::Sub | Op::Mul | Op::Div => Ok(Kind::Arrow(0, Box::new(Kind::Star(Type::Func(
	                    Box::new(Type::Free(0)),
	                    Box::new(Type::Func(
	                        Box::new(Type::Free(0)),
	                        Box::new(Type::Free(0)),
	                    )),
	                ))))),
	            }
	        }
	        &Node::FunCall(ref lamdba, ref arg) => {
	        	let l = if let Kind::Arrow(id, k) = Kind::of(lamdba, env)? {

	        	}
	        	match (Kind::of(lamdba, env)?, Kind::of(arg, env)?) {
	        		(Kind::Star(Star::Func(p, r)), Kind::Star(a)) => match p == a {
	        			true => Ok(Star(r)),
	        			false => Err("Parameter type missmatch."),
	        		},
	                (Kind::Arrow(id, k), Kind::Star(a)) => {}
	        		(_, Kind::Star(_)) => Err("Attempted to call non-function."),
	        		(_, _) => Err("Higher kinded types not supported."),
	        	}
	        },
	   		_ => Err("Unimplemented expression."),
		}
	}

	pub fn kind_of(n: &Node) -> Result<Kind, &'static str> {
		Kind::of(n, &TypeEnv{var: "", val: Kind::Star(Type::Unit), tail: None})
	}

	fn subst(self, id: TId, ty: &Type) -> Kind {
		match self {
			Kind::Star(t) => Kind::Star(t.subst(id, ty)),
			k @ Kind::Arrow(i, _) if i == id => k,
			Kind::Arrow(i, k) if i != id => k.subst(id, ty)
		}
	}

	fn unify(self, ty: &Type) -> (Kind, Option<(TId, Type)>) {
		
	}
}

impl Type {
    fn subst(self, id: TId, ty: &Type) -> Type {
    	match self {
    		Type::Free(i) if i == id => ty.clone(),
    		Type::Func(p, r) => Type::Func(Box::new(p.subst(id, ty)), Box::new(r.subst(id, ty))),
    		t => t
    	}
    }

    fn unify_param(self, ty: &Type) -> Result(Type, Option<(TId, Type)>) {
    	match self {
    	    Type::Func(t, p) => expr,
    	    None => expr,
    	}
    }

    fn unify(self, ty: &Type) -> 
}