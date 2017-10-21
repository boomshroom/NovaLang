use super::parser::{self, Node, Literal, Pattern, Op};
use std::collections::{HashMap, HashSet};
use std::iter;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TId(u64);

impl TId {
    fn new() -> TId {
        TId(0)
    }
    fn next(&mut self) -> Type {
        let t = self.0;
        self.0 += 1;
        Type::Free(TId(t))
    }
}

type Ident = String;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Unit,
    Int,
    Bool,
    Func(Box<Type>, Box<Type>),
    Free(TId),
}

type Scheme = (Vec<TId>, Type);

trait Types {
    fn ftv(&self) -> HashSet<TId>;
    fn apply(self, &Subst) -> Self;
}

impl Types for Type {
    fn ftv(&self) -> HashSet<TId> {
        match self {
            &Type::Free(id) => iter::once(id).collect(),
            &Type::Func(ref p, ref r) => &p.ftv() | &r.ftv(),
            _ => HashSet::new(),
        }
    }

    fn apply(self, s: &Subst) -> Type {
        match self {
            Type::Free(id) => s.get(&id).map(Clone::clone).unwrap_or(Type::Free(id)),
            Type::Func(p, r) => Type::Func(Box::new(p.apply(s)), Box::new(r.apply(s))),
            t => t,
        }
    }
}

impl Types for Scheme {
    fn ftv(&self) -> HashSet<TId> {
        let &(ref v, ref t) = self;
        &t.ftv() - &v.iter().cloned().collect()
    }

    fn apply(self, s: &Subst) -> Scheme {
        let (v, t) = self;
        let mut s = s.clone();
        v.iter().for_each(|id| { s.remove(id); });
        (v, t.apply(&s))
    }
}

impl<T> Types for Vec<T>
where
    T: Types,
{
    fn ftv(&self) -> HashSet<TId> {
        self.iter().map(Types::ftv).fold(
            HashSet::new(),
            |a, b| &a | &b,
        )
    }
    fn apply(self, s: &Subst) -> Vec<T> {
        self.into_iter().map(|t| t.apply(s)).collect()
    }
}

type Subst = HashMap<TId, Type>;

fn composeSubst(left: Subst, right: Subst) -> Subst {
    right
        .into_iter()
        .map(|(k, v)| (k, v.apply(&left)))
        .chain(left.clone().into_iter())
        .collect()
}

#[derive(Debug, Clone)]
struct TypeEnv(HashMap<Ident, Scheme>);

impl TypeEnv {
    fn remove_mut(&mut self, i: &str) {
        self.0.remove(i);
    }

    fn remove(&self, i: &str) -> TypeEnv {
        let mut x = self.0.clone();
        x.remove(i);
        TypeEnv(x)
    }

    fn generalize(&self, t: Type) -> Scheme {
        let vars = &t.ftv() - &self.ftv();
        (vars.into_iter().collect(), t)
    }
}

impl Types for TypeEnv {
    fn ftv(&self) -> HashSet<TId> {
        self.0.values().map(Types::ftv).fold(
            HashSet::new(),
            |a, b| &a | &b,
        )
    }
    fn apply(self, s: &Subst) -> TypeEnv {
        TypeEnv(self.0.into_iter().map(|(i, t)| (i, t.apply(s))).collect())
    }
}

fn instantiate(s: &Scheme, next: &mut TId) -> Type {
    let &(ref v, ref t) = s;
    let s = v.iter().map(|t| (t.clone(), next.next())).collect();
    t.clone().apply(&s)
}

#[derive(Debug)]
pub enum TypeError {
    Unbound(Ident),
    NoUnify(Type, Type),
    Occurs(Type, TId),
    Wrapped(Node, Box<TypeError>),
    Unimplemented(Node),
}

fn litT(l: &Literal) -> Type {
    match l {
        &Literal::Unit => Type::Unit,
        &Literal::Int(_) => Type::Int,
        &Literal::True | &Literal::False => Type::Bool,
    }
}

impl Type {
    fn unify(self, other: Type, next: &mut TId) -> Result<Subst, TypeError> {
        match (self, other) {
            (Type::Func(l1, r1), Type::Func(l2, r2)) => {
                let s1 = l1.unify(*l2, next)?;
                let s2 = r1.apply(&s1).unify(r2.apply(&s1), next)?;
                Ok(composeSubst(s1, s2))
            }
            (Type::Free(id), t) |
            (t, Type::Free(id)) => t.varBind(id),
            (t1, t2) => {
                match t1 == t2 {
                    true => Ok(HashMap::new()),
                    false => Err(TypeError::NoUnify(t1, t2)),
                }
            }
        }
    }

    fn varBind(self, id: TId) -> Result<Subst, TypeError> {
        if let Type::Free(i) = self {
            Ok(match i == id {
                true => HashMap::new(),
                false => iter::once((id, Type::Free(i))).collect(),
            })
        } else if self.ftv().contains(&id) {
            Err(TypeError::Occurs(self, id))
        } else {
            Ok(iter::once((id, self)).collect())
        }
    }
}


fn infer(n: &Node, env: &mut TypeEnv, next: &mut TId) -> Result<(Subst, Type), TypeError> {
    match n {
        &Node::Ident(ref i) => {
            match env.0.get(i) {
                Some(s) => Ok((HashMap::new(), instantiate(s, next))),
                None => Err(TypeError::Unbound(i.clone())),
            }
        }
        &Node::Lit(ref l) => Ok((HashMap::new(), litT(l))),
        &Node::Lambda(Pattern::Ident(ref i), _, ref e) => {
            let tv = next.next();
            //let env = env.remove(i.as_str());
            let old = env.0.insert(i.clone(), (Vec::new(), tv.clone()));
            let (s, t) = infer(e, env, next)?;
            match old {
                Some(t) => env.0.insert(i.clone(), t),
                None => env.0.remove(i.as_str()),
            };
            let t = Type::Func(Box::new(tv.apply(&s)), Box::new(t));
            Ok((s, t))
        }
        &Node::FunCall(ref e1, ref e2) => {
            let err = |e| TypeError::Wrapped(Node::FunCall(e1.clone(), e2.clone()), Box::new(e));
            let t = next.next();
            let (s1, t1) = match infer(e1, env, next) {
                Ok(a) => a,
                Err(e) => return Err(err(e)),
            };
            let (s2, t2) = match infer(e2, &mut env.clone().apply(&s1), next) {
                Ok(a) => a,
                Err(e) => return Err(err(e)),
            };
            let s3 = match t1.apply(&s2).unify(
                Type::Func(Box::new(t2), Box::new(t.clone())),
                next,
            ) {
                Ok(a) => a,
                Err(e) => return Err(err(e)),
            };
            let t = t.apply(&s3);
            Ok((composeSubst(composeSubst(s3, s2), s1), t))
        }
        &Node::Let(ref ps, ref e) => {
            let mut s = HashMap::new();
            let mut env = env.clone();

            for &(ref p, ref b) in ps.iter() {
                if let &Pattern::Ident(ref i) = p {
                    let (s1, t1) = infer(b, &mut env, next)?;
                    let t = env.clone().apply(&s1).generalize(t1);
                    env.0.insert(i.clone(), t);

                    env = env.apply(&s1);
                    s = composeSubst(s, s1);
                } else {
                    return Err(TypeError::Unimplemented(Node::Let(ps.clone(), e.clone())));
                }
            }
            let (s2, t) = infer(e, &mut env, next)?;
            return Ok((composeSubst(s, s2), t));
        }
        &Node::Op(ref o) => {
            match o.clone() {
                Op::Add | Op::Sub | Op::Mul | Op::Div => {
                    let t = next.next();
                    Ok((
                        HashMap::new(),
                        Type::Func(
                            Box::new(t.clone()),
                            Box::new(Type::Func(Box::new(t.clone()), Box::new(t))),
                        ),
                    ))
                }
            }
        },
        &Node::If(ref c, ref t, ref f) => {
            //let n = next.next();
            //let fun = Type::Func(Box::new(Type::Bool), Box::new(Type::Func(Box::new(n.clone()), Box::new(Type::Func(Box::new(n.clone()), Box::new(n.clone()))))));

            let (s, tc) = infer(c, env, next)?;

            let sc = composeSubst(s, tc.clone().unify(Type::Bool, next)?);
            let tc = tc.apply(&sc);
            if tc != Type::Bool {
                return Err(TypeError::Wrapped(*c.clone(), Box::new(TypeError::Wrapped(Node::If(c.clone(), t.clone(), f.clone()), Box::new(TypeError::NoUnify(Type::Bool, tc))))));
            };
            let (s1, t1) = infer(t, &mut env.clone().apply(&sc), next)?;

            let (s2, t2) = infer(f, &mut env.clone().apply(&s1), next)?;

            let s3 = t1.clone().unify(t2, next)?;
            let t = t1.clone().apply(&s3);
            Ok((composeSubst(s3, composeSubst(composeSubst(sc, s1), s2)), t))
        }
        exp => Err(TypeError::Unimplemented(exp.clone())),
    }
}

pub fn run_infer(n: &Node) -> Result<Type, TypeError> {
    let mut ids = TId::new();
    infer(n, &mut TypeEnv(HashMap::new()), &mut ids).map(|(_, t)| t)
}
