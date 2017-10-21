use super::parser::{self, Node, Literal, Pattern, Op};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Unit,
    Int,
    Bool,
    Func(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Box(Box<Type>),
    Free(u64),
}

pub struct TypeEnv<'a> {
	var: &'a str,
	val: Type,
	tail: Option<&'a TypeEnv<'a>>,
}

impl<'a> From<&'a parser::Type> for Type {
	fn from(t: &'a parser::Type) -> Type {
		match *t {
			parser::Type::Unit => Type::Unit,
			parser::Type::Int => Type::Int,
			parser::Type::Bool => Type::Bool,
			parser::Type::Func(ref a, ref b) => Type::Func(Box::new(a.as_ref().into()), Box::new(b.as_ref().into())),
			parser::Type::Tuple(ref t) => Type::Tuple(t.iter().map(From::from).collect()),
            parser::Type::Box(ref t) => Type::Box(Box::new(t.as_ref().into())),
		}
	}
}

impl From<parser::Type> for Type {
	fn from(t: parser::Type) -> Type {
		match t {
			parser::Type::Unit => Type::Unit,
			parser::Type::Int => Type::Int,
			parser::Type::Bool => Type::Bool,
			parser::Type::Func(a, b) => Type::Func(Box::new((*a).into()), Box::new((*b).into())),
			parser::Type::Tuple(t) => Type::Tuple(t.into_iter().map(From::from).collect()),
            parser::Type::Box(box t) => Type::Box(Box::new(t.into())),
		}
	}
}

fn lit_type(l: &Literal) -> Type {
	match l {
		&Literal::Unit => Type::Unit,
		&Literal::Int(_) => Type::Int,
		&Literal::True | &Literal::False => Type::Bool,
	}
}

pub fn check<'a>(expr: &Node, env: &'a TypeEnv<'a>, nextId: &mut u64) -> Result<Type, &'static str> {
    match expr {
        &Node::Lit(ref v) => Ok(lit_type(v)),
        &Node::FunCall(ref lamdba, ref arg) => {
        	match (check(lamdba, env, nextId)?, check(arg, env, nextId)?) {
        		(l @ Type::Func(_, _), a) => {
                    let id = *nextId;
                    *nextId += 1;
        			match l.unify(&Type::Func(Box::new(a), Box::new(Type::Free(id)))) {
                        Ok(Type::Func(_, r)) => Ok(*r),
                        Err(e) => Err(e),
                        Ok(t) => Err("Function unified as non function."),
                    }
        		}
                (Type::Free(_), a) => {
                    let id = *nextId;
                    *nextId += 1;
                    Ok(Type::Free(id))
                }
        		(_, _) => Err("Attempted to call non-function."),
        	}
        },
        &Node::Ident(ref i) => {
            match i.as_str() {
                "succ" | "pred" => Ok(Type::Func(Box::new(Type::Int), Box::new(Type::Int))),
                "iszero" => Ok(Type::Func(Box::new(Type::Int), Box::new(Type::Bool))),
                "Box" => Ok(Type::Func(
                    Box::new(Type::Free(0)),
                    Box::new(
                        Type::Box(Box::new(Type::Free(0))),
                    ),
                )),
                "mapB" => Ok(Type::Func(
                    Box::new(Type::Func(
                        Box::new(Type::Free(1)),
                        Box::new(Type::Free(1)),
                    )),
                    Box::new(Type::Func(
                        Box::new(Type::Box(Box::new(Type::Free(1)))),
                        Box::new(Type::Box(Box::new(Type::Free(1)))),
                    )),
                )),
                "unwrap" => Ok(Type::Func(Box::new(Type::Box(Box::new(Type::Free(2)))), Box::new(Type::Free(2)))),
                i => env.lookup(i).cloned().ok_or("Value undefined."),
            }
        }
        &Node::If(ref c, ref t, ref f) => {
            match (check(c, env, nextId)?, check(t, env, nextId)?, check(f, env, nextId)?) {
                (Type::Bool, t, f) => {
                    if t == f {
                        Ok(t)
                    } else {
                        Err("Different branches of if must have the same type.")
                    }
                }
                //(Type::Bool, ref t, ref f) if t != f => Err("Different branches of if must have the same type."),
                (_, _, _) => Err("If expression must have boolean condition."),
            }
        }
        &Node::Op(o) => {
            match o {
                Op::Add | Op::Sub | Op::Mul | Op::Div => Ok(Type::Func(
                    Box::new(Type::Free(3)),
                    Box::new(Type::Func(
                        Box::new(Type::Free(3)),
                        Box::new(Type::Free(3)),
                    )),
                )),
            }
        }
        &Node::Tuple(ref e) => {
            e.iter()
                .map(|e| check(e, env, nextId))
                .collect::<Result<Vec<Type>, &'static str>>()
                .map(Type::Tuple)
        }
        &Node::Let(ref bs, ref e) => {
        	//if bs.len() > 1 { return Err("Multiple bindings not typechecked yet") };
        	let bs = bs.as_slice();

        	env.check_let_body(bs, e, nextId)
/*
        	let mut envs = Vec::with_capacity(bs.len());
        	let mut env = env;
        	for &(ref p, ref b) in bs.iter() {
				if let &Pattern::Ident(ref i) = p {
					let e = Box::new(TypeEnv{var: i.as_str(), val: check(b, env)?, tail: Some(&env)});
	        		//env = e.as_ref();
	        		envs.push(e);
	        		env = envs.last().unwrap().as_ref()
	        	} else {
	        		return Err("Only simple let bindings typechecked yet.")
	        	}
        	}
			check(e, env)*/
        }
        &Node::Lambda(Pattern::Ident(ref i), _, ref v) => {
        	//let p = t.as_ref().map(Into::into).ok_or("Parameter types must be specified.")?;
            let p = Type::Free(*nextId);
            *nextId += 1;
        	let env = TypeEnv{var: i, val: p, tail: Some(env)};
        	let b = check(v, &env, nextId)?;
        	Ok(Type::Func(Box::new(env.val), Box::new(b)))
        },
        _ => Err("Unimplemented expression for typechecking."),
    }
}

impl TypeEnv<'static> {
	pub fn new() -> TypeEnv<'static> {
		TypeEnv{var: "", val: Type::Unit, tail: None}
	}
}

impl <'a> TypeEnv<'a> {
	fn lookup(&'a self, i: &str) -> Option<&'a Type> {
		match self.var == i {
			true => Some(&self.val),
			false => self.tail.and_then(|e| e.lookup(i)),
		}
	}

	fn add(&'a self, i: &'a str, val: Type) -> TypeEnv<'a> {
		TypeEnv::<'a>{var: i, val: val, tail: Some(self)}
	}

	fn add_node(&'a self, i: &'a str, val: &Node, nextId: &mut u64) -> Result<TypeEnv<'a>, &'static str> {
		check(&val, self, nextId).map(|t| self.add(i, t))
	}

	fn check_let_body(&self, vals: &[(Pattern, Node)], body: &Node, nextId: &mut u64) -> Result<Type, &'static str> {
		match vals.len() {
			0 => check(body, self, nextId),
			_ => {
				let (ref p, ref b) = vals[0];
				match p {
					&Pattern::Ident(ref i) => self.add_node(i.as_str(), b, nextId).and_then(|env| env.check_let_body(&vals[1..], body, nextId)),
					_ => return Err("Only simple let bindings typechecked yet."),
				}
				
			}
		}
	}
}

impl Type {
    fn unify<'a>(&'a self, other: &'a Type) -> Result<(Type, HashMap<>), &'static str> {
        println!("({:?}) =? ({:?})", self, other);
        match (self, other) {
            (&Type::Free(i1), &Type::Free(i2)) => Ok(Type::Free(::std::cmp::min(i1, i2))),
            (t, f @ &Type::Free(_)) | (f @ &Type::Free(_), t) => match t.has(f) {
                true => Err("Recursive Type."),
                false => Ok(t.clone()),
            },
            (&Type::Box(ref t1), &Type::Box(ref t2)) => Ok(Type::Box(Box::new(t1.unify(t2.as_ref())?))),
            (&Type::Tuple(ref ts1), &Type::Tuple(ref ts2)) => {
                if ts1.len() != ts2.len() {
                    return Err("Tuples of different lengths.");
                }
                let mut tup = Type::Tuple(ts1.iter().zip(ts2.iter()).map(|(t1, t2)| t1.unify(t2)).collect::<Result<Vec<Type>, &'static str>>()?);
                for i in ts1.iter().zip(ts2.iter()) {
                    match i {
                        (t, &Type::Free(id)) | (&Type::Free(id), t) => tup = tup.replace_id(t, id),
                        _ => {},
                    }
                }
                Ok(tup)
            },
            (&Type::Func(ref p1, ref r1), &Type::Func(ref p2, ref r2)) => {
                let p = p1.unify(&*p2)?;
                let r = match (p1.as_ref(), p2.as_ref(), r1.unify(r2.as_ref())?) {
                    (&Type::Free(id), p, r) | (p, &Type::Free(id), r) => r.replace_id(p, id),
                    (_, _, r) => r,
                };
                Ok(Type::Func(Box::new(p), Box::new(r)))
            },
            (t1, t2) => {
                match t1 == t2 {
                    true => Ok(t1.clone()),
                    false => Err("Unmatched types."),
                }
            }
        }
    }

    fn replace_id(&self, other: &Type, id: u64) -> Type {
        match self {
            &Type::Func(ref p, ref b) => Type::Func(Box::new(p.replace_id(other, id)), Box::new(b.replace_id(other, id))),
            &Type::Box(ref t) => Type::Box(Box::new(t.replace_id(other, id))),
            &Type::Tuple(ref ts) => Type::Tuple(ts.iter().map(|t| t.replace_id(other, id)).collect()),
            &Type::Free(f) if f == id => other.clone(),
            t => t.clone(),
        }
    }

    fn has(&self, other: &Type) -> bool {
        if self == other {
            return true;
        }
        match self {
            &Type::Func(ref p, ref b) => p.has(other),
            &Type::Box(ref t) => t.has(other),
            &Type::Tuple(ref ts) => ts.iter().any(|t| t.has(other)),
            _ => false,
        }
    }
}
