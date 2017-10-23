use super::parser::{Node, Literal, Pattern, Op};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::iter;
use std::cmp::Ordering;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TId(u64);

impl TId {
    fn new() -> TId {
        TId(0)
    }
    fn next_t(&mut self) -> Type {
        let t = self.0;
        self.0 += 1;
        Type::Free(TId(t))
    }
    fn next_id(&mut self) -> TId {
        let t = self.0;
        self.0 += 1;
        TId(t)
    }
}

type Ident = String;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type {
    Unit,
    Int,
    Bool,
    Func(Box<Type>, Box<Type>),
    Closure(Box<Type>, Box<Type>, Vec<Type>),
    Free(TId),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Scheme {
    Type(Type),
    Forall(Type, Vec<(TId, HashSet<Class>)>),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Class {
    Num,
    Eq,
    Func(Type, Type),
}

// type Scheme = (Vec<TId>, Type);

trait Types {
    fn ftv(&self) -> HashSet<TId>;
    fn apply(self, &TypeInfo) -> Self;
}

impl Types for Type {
    fn ftv(&self) -> HashSet<TId> {
        match self {
            &Type::Free(id) => iter::once(id).collect(),
            &Type::Func(ref p, ref r) => &p.ftv() | &r.ftv(),
            &Type::Closure(ref p, ref r, ref hs) => &(&p.ftv() | &r.ftv()) | &hs.ftv(),
            _ => HashSet::new(),
        }
    }

    fn apply(self, s: &TypeInfo) -> Type {
        match self {
            Type::Free(id) => s.subst.get(&id).cloned().unwrap_or(Type::Free(id)),
            Type::Func(p, r) => Type::Func(Box::new(p.apply(s)), Box::new(r.apply(s))),
            Type::Closure(p, r, hs) => {
                Type::Closure(Box::new(p.apply(s)), Box::new(r.apply(s)), hs.apply(s))
            }
            t => t,
        }
    }
}

impl Types for Scheme {
    fn ftv(&self) -> HashSet<TId> {
        match self {
            &Scheme::Type(ref t) => t.ftv(),
            &Scheme::Forall(ref t, ref v) => {
                &t.ftv() - &v.iter().map(|&(var, _)| var.clone()).collect()
            } // TODO How to use the constraignts.
        }
    }

    fn apply(self, s: &TypeInfo) -> Scheme {
        match self {
            Scheme::Type(t) => Scheme::Type(t.apply(s)),
            Scheme::Forall(t, v) => {
                let mut s = s.clone();
                v.iter().for_each(|&(id, _)| {
                    s.remove(id);
                });
                Scheme::Forall(t.apply(&s),
                               v.into_iter().map(|(id, c)| (id, c.apply(&s))).collect())
            }
        }
    }
}

impl<T> Types for Vec<T>
    where T: Types
{
    fn ftv(&self) -> HashSet<TId> {
        self.iter().map(Types::ftv).fold(HashSet::new(), |a, b| &a | &b)
    }
    fn apply(self, s: &TypeInfo) -> Vec<T> {
        self.into_iter().map(|t| t.apply(s)).collect()
    }
}

impl Types for Class {
    fn ftv(&self) -> HashSet<TId> {
        match self {
            &Class::Func(ref p, ref r) => &p.ftv() | &r.ftv(),
            _ => HashSet::new(),
        }
    }

    fn apply(self, info: &TypeInfo) -> Class {
        match self {
            Class::Func(p, r) => Class::Func(p.apply(info), r.apply(info)),
            c => c,
        }
    }
}

impl<T: Eq + Hash> Types for HashSet<T>
    where T: Types
{
    fn ftv(&self) -> HashSet<TId> {
        self.iter().map(Types::ftv).fold(HashSet::new(), |a, b| &a | &b)
    }
    fn apply(self, s: &TypeInfo) -> HashSet<T> {
        self.into_iter().map(|t| t.apply(s)).collect()
    }
}

#[derive(Debug,Clone)]
struct TypeInfo {
    subst: HashMap<TId, Type>,
    constr: HashMap<TId, HashSet<Class>>,
}
// type Subst = HashMap<TId, Type>;
//
// fn compose(left: Subst, right: Subst) -> Subst {
// right
// .into_iter()
// .map(|(k, v)| (k, v.apply(&left)))
// .chain(left.clone().into_iter())
// .collect()
// }
//
#[derive(Debug, Clone)]
struct TypeEnv(HashMap<Ident, Scheme>);

impl TypeEnv {
    fn generalize(&self, t: Type, info: &TypeInfo) -> Scheme {
        let vars = &t.ftv() - &self.ftv();
        match vars.len() {
            0 => Scheme::Type(t),
            n => {
                Scheme::Forall(t,
                               vars.into_iter()
                                   .map(|id| {
                        let cls =
                            info.constr.get(&id).into_iter().flat_map(|cls| cls.clone()).collect();
                        (id, cls)
                    })
                                   .collect())
            }
        }
    }
}

impl Types for TypeEnv {
    fn ftv(&self) -> HashSet<TId> {
        self.0.values().map(Types::ftv).fold(HashSet::new(), |a, b| &a | &b)
    }
    fn apply(self, s: &TypeInfo) -> TypeEnv {
        TypeEnv(self.0.into_iter().map(|(i, t)| (i, t.apply(s))).collect())
    }
}

struct Constraints(HashMap<TId, HashSet<Class>>);

impl TypeInfo {
    fn new() -> TypeInfo {
        TypeInfo {
            subst: HashMap::new(),
            constr: HashMap::new(),
        }
    }

    fn compose(self, other: TypeInfo) -> TypeInfo {
        let TypeInfo { subst: s, constr: c } = other;
        let subst = s.into_iter()
            .map(|(k, v)| (k, v.apply(&self)))
            .chain(self.subst.clone().into_iter())
            .collect();

        let constr = c.clone()
            .into_iter()
            .chain(self.constr.iter().map(|(id, cl1)| match c.get(id) {
                Some(ref cl2) => (id.clone(), cl1 | cl2),
                None => (id.clone(), cl1.clone()),
            }))
            .collect();

        TypeInfo {
            subst: subst,
            constr: constr,
        }
    }

    fn apply_constraints(self) -> Result<TypeInfo, TypeError> {
        let TypeInfo { constr, subst } = self.clone();
        constr.into_iter().fold(Ok(self), |info, (id, cls)| match subst.get(&id) {
            Some(t) => {
                cls.clone().into_iter().fold(info, |info, cl| {
                    info.and_then(|info| {
                        t.clone().unify_class(cl.apply(&info)).map(|i| i.compose(info))
                    })
                })
            }
            None => info,
        })
    }

    fn remove(&mut self, id: TId) {
        self.subst.remove(&id);
        self.constr.remove(&id);
    }

    fn test_constraints(&self) -> Result<(), TypeError> {
        let &TypeInfo { ref subst, ref constr } = self;
        for (id, ty) in subst.iter() {
            match constr.get(id) {
                Some(cs) => {
                    for c in cs.iter() {
                        if !ty.in_class(c) {
                            return Err(TypeError::Constrained(ty.clone(), c.clone()));
                        }
                    }
                }
                None => {}
            }
        }
        Ok(())
    }

    fn constrain(self, id: &Type, class: Class) -> TypeInfo {
        let mut s = self;
        if let &Type::Free(id) = id {
            s.constr.entry(id).or_insert(HashSet::new()).insert(class);
        }
        s
    }
}

fn instantiate(s: &Scheme, next: &mut TId) -> (TypeInfo, Type) {
    match s {
        &Scheme::Type(ref t) => (TypeInfo::new(), t.clone()),
        &Scheme::Forall(ref t, ref v) => {
            println!("{:?} {{{:?}}}", t, v);
            let (s, c): (_, HashMap<_, _>) = v.iter()
                .map(|&(ref t, ref cls)| {
                    let id = next.next_id();
                    ((t.clone(), Type::Free(id)), (id, cls.clone()))
                })
                .unzip();
            let info = TypeInfo {
                subst: s,
                constr: c.clone(),
            };
            let t = t.clone().apply(&info);
            let info = TypeInfo {
                subst: HashMap::new(),
                constr: c.into_iter().map(|(id, c)| (id, c.apply(&info))).collect(),
            };

            (info, t)
        }
    }
}

#[derive(Debug)]
pub enum TypeError {
    Unbound(Ident),
    NoUnify(Type, Type),
    Recursive(Type, TId),
    Wrapped(Node, Box<TypeError>),
    Unimplemented(Node),
    Constrained(Type, Class),
}

fn lit_type(l: &Literal) -> Type {
    match l {
        &Literal::Unit => Type::Unit,
        &Literal::Int(_) => Type::Int,
        &Literal::True | &Literal::False => Type::Bool,
    }
}

impl Type {
    fn unify(self, other: Type) -> Result<TypeInfo, TypeError> {
        match (self, other) {
            (Type::Func(l1, r1), Type::Func(l2, r2)) => {
                let s1 = l1.unify(*l2)?;
                let s2 = Type::unify(r1.apply(&s1), r2.apply(&s1))?;
                Ok(s1.compose(s2))
            }
            (Type::Closure(p1, r1, h1), Type::Closure(p2, r2, h2)) => {
                if h1.len() != h2.len() {
                    return Err(TypeError::NoUnify(Type::Closure(p1, r1, h1),
                                                  Type::Closure(p2, r2, h2)));
                }
                let s1 = p1.unify(*p2)?;
                let s2 = Type::unify(r1.apply(&s1), r2.apply(&s1))?;

                h1.into_iter().zip(h2.into_iter()).fold(Ok(s1.compose(s2)), |s, (t1, t2)| {
                    s.and_then(|s| Ok(Type::unify(t1.apply(&s), t2.apply(&s))?.compose(s)))
                })
            }
            (Type::Free(id), t) |
            (t, Type::Free(id)) => t.var_bind(id),
            (t1, t2) => {
                match t1 == t2 {
                    true => Ok(TypeInfo::new()),
                    false => Err(TypeError::NoUnify(t1, t2)),
                }
            }
        }
    }

    fn unify_class(self, class: Class) -> Result<TypeInfo, TypeError> {
        match (self, class) {
            (t @ Type::Free(_), c) => Ok(TypeInfo::new().constrain(&t, c)),
            (Type::Func(pf, rf), Class::Func(pc, rc)) => {
                let i1 = pf.unify(pc)?;
                let i2 = Type::unify(rf.apply(&i1), rc.apply(&i1))?;
                Ok(i1.compose(i2))
            }
            (Type::Closure(pf, rf, _), Class::Func(pc, rc)) => {
                let i1 = pf.unify(pc)?;
                let i2 = Type::unify(rf.apply(&i1), rc.apply(&i1))?;
                Ok(i1.compose(i2))
            }
            (Type::Int, Class::Num) |
            (Type::Int, Class::Eq) |
            (Type::Bool, Class::Eq) |
            (Type::Unit, Class::Eq) => Ok(TypeInfo::new()),
            (t, c) => Err(TypeError::Constrained(t, c)),
        }
    }

    fn var_bind(self, id: TId) -> Result<TypeInfo, TypeError> {
        let mut info = TypeInfo::new();
        if let Type::Free(i) = self {
            match i.cmp(&id) {
                Ordering::Equal => {}
                Ordering::Less => {
                    info.subst.insert(id, Type::Free(i));
                }
                Ordering::Greater => {
                    info.subst.insert(i, Type::Free(id));
                }
            };
        } else if self.contains(id) {
            return Err(TypeError::Recursive(self, id));
        } else {
            info.subst.insert(id, self);
        }
        Ok(info)
    }

    fn contains(&self, id: TId) -> bool {
        match self {
            &Type::Free(i) => id == i,
            &Type::Func(ref p, ref r) => p.contains(id) | r.contains(id),
            &Type::Closure(ref p, ref r, ref hs) => {
                p.contains(id) | r.contains(id) | hs.iter().any(|h| h.contains(id))
            }
            _ => false,
        }
    }

    fn in_class(&self, c: &Class) -> bool {
        match (self, c) {
            (&Type::Free(_), _) => true,
            (&Type::Int, &Class::Num) => true,
            (&Type::Int, &Class::Eq) |
            (&Type::Bool, &Class::Eq) |
            (&Type::Unit, &Class::Eq) => true,
            (&Type::Func(ref p1, ref r1), &Class::Func(ref p2, ref r2)) => {
                if let Ok(i1) = p1.clone().unify(p2.clone()) {
                    r1.clone().apply(&i1).unify(r2.clone().apply(&i1)).is_ok()
                } else {
                    false
                }
            }
            (&Type::Closure(ref p1, ref r1, _), &Class::Func(ref p2, ref r2)) => {
                if let Ok(i1) = p1.clone().unify(p2.clone()) {
                    r1.clone().apply(&i1).unify(r2.clone().apply(&i1)).is_ok()
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

fn free_vars(n: &Node) -> HashSet<String> {
    match n {
        &Node::Ident(ref i) => iter::once(i.clone()).collect(),
        &Node::Lit(_) => HashSet::new(),
        &Node::Lambda(Pattern::Ident(ref i), _, ref e) => {
            &free_vars(e) - &iter::once(i.clone()).collect()
        }
        &Node::FunCall(ref f, ref a) => &free_vars(f) | &free_vars(a),
        &Node::Let(ref ps, ref e) => {
            if ps.len() != 1 {
                panic!("Unimplemented: multiple let bindings.")
            };
            let (i, b) = match ps.first() {
                Some(&(Pattern::Ident(ref i), ref b)) => (i, b),
                p => panic!("Unimplemented pattern: {:?}", p),
            };
            let free_b = free_vars(b);
            let free_e = free_vars(e);
            let decled = iter::once(i.clone()).collect();
            &free_b | &(&free_e - &decled)
        }
        &Node::Op(_) => HashSet::new(),
        &Node::If(ref c, ref t, ref f) => &(&free_vars(c) | &free_vars(t)) | &free_vars(f),
        n => panic!("Unimplemented: {:?}", n),
    }
}

fn infer(n: &Node, env: &mut TypeEnv, next: &mut TId) -> Result<(TypeInfo, Type), TypeError> {
    match n {
        &Node::Ident(ref i) => {
            env.0
                .get(i)
                .map(|s| instantiate(s, next))
                .ok_or(TypeError::Unbound(i.clone()))
        }
        &Node::Lit(ref l) => Ok((TypeInfo::new(), lit_type(l))),
        &Node::Lambda(Pattern::Ident(ref i), _, ref e) => {
            let new_id = next.next_t();
            // let env = env.remove(i.as_str());
            let old = env.0.insert(i.clone(), (Scheme::Type(new_id.clone())));

            let (s, t) = infer(e, env, next)?;

            match old {
                Some(t) => env.0.insert(i.clone(), t),
                None => env.0.remove(i),
            };

            let mut free = free_vars(e);
            free.remove(i);
            if free.is_empty() {
                let t = Type::Func(Box::new(new_id.apply(&s)), Box::new(t));
                Ok((s, t))
            } else {
                let schemes = free.into_iter()
                    .map(|i| {
                        env.0
                            .get(&i)
                            .map(|s1| instantiate(s1, next))
                            .ok_or(TypeError::Unbound(i))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let (infos, ts): (Vec<_>, Vec<_>) = schemes.into_iter().unzip();
                let info = infos.into_iter().fold(s, |i1, i2| i1.compose(i2));
                let ts = ts.into_iter().map(|t| t.apply(&info)).collect();
                let t = Type::Closure(Box::new(new_id.apply(&info)), Box::new(t), ts);

                Ok((info, t))
            }
        }
        &Node::FunCall(ref f, ref a) => {
            let err = |e| TypeError::Wrapped(Node::FunCall(f.clone(), a.clone()), Box::new(e));
            (|f, a| {
                    let t = next.next_t();

                    let (s1, tf) = infer(f, env, next)?;
                    let (s2, ta) = infer(a, &mut env.clone().apply(&s1), next)?;

                    let sc = tf.clone().unify_class(Class::Func(ta, t.clone()))?;
                    let s = s1.compose(s2).compose(sc).apply_constraints()?;

                    let t = t.apply(&s);
                    Ok((s, t))
                })(f, a)
                .map_err(err)
        }
        &Node::Let(ref ps, ref e) => {
            let mut s = TypeInfo::new();
            let mut env = env.clone();

            for &(ref p, ref b) in ps.iter() {
                if let &Pattern::Ident(ref i) = p {
                    let (s1, t1) = infer(b, &mut env, next)?;
                    let t = env.clone().apply(&s1).generalize(t1, &s1);
                    env.0.insert(i.clone(), t);

                    env = env.apply(&s1);
                    s = s.compose(s1);
                } else if let &Pattern::Constructor(ref i, ref v) = p {
                    if v.len() != 0 || i.starts_with(char::is_uppercase) {
                        return Err(TypeError::Unimplemented(Node::Let(ps.clone(), e.clone())));
                    }
                    let (s1, t1) = infer(b, &mut env, next)?;
                    let t = env.clone().apply(&s1).generalize(t1, &s1);
                    env.0.insert(i.clone(), t);

                    env = env.apply(&s1);
                    s = s.compose(s1);
                } else {
                    return Err(TypeError::Unimplemented(Node::Let(ps.clone(), e.clone())));
                }
            }
            let (s2, t) = infer(e, &mut env, next)?;
            let info = s.compose(s2).apply_constraints()?;
            let t = t.apply(&info);
            return Ok((info, t));
        }
        &Node::Op(o) => {
            match o {
                Op::Add | Op::Sub | Op::Mul | Op::Div => {
                    let t = next.next_t();
                    Ok((TypeInfo::new().constrain(&t, Class::Num),
                        Type::Func(Box::new(t.clone()),
                                   Box::new(Type::Closure(Box::new(t.clone()),
                                                          Box::new(t.clone()),
                                                          vec![t])))))
                }
                Op::Equal => {
                    let t = next.next_t();
                    Ok((TypeInfo::new().constrain(&t, Class::Eq),
                        Type::Func(Box::new(t.clone()),
                                   Box::new(Type::Closure(Box::new(t.clone()),
                                                          Box::new(Type::Bool),
                                                          vec![t])))))
                }
            }
        }
        &Node::If(ref c, ref t, ref f) => {
            // let n = next.next();
            // let fun = Type::Func(Box::new(Type::Bool),
            // Box::new(Type::Func(Box::new(n.clone()),
            // Box::new(Type::Func(Box::new(n.clone()),
            // Box::new(n.clone()))))));
            //


            let (s, tc) = infer(c, env, next)?;

            let sc = s.compose(tc.unify(Type::Bool)?);

            let (s1, t_true) = infer(t, &mut env.clone().apply(&sc), next)?;

            let (s2, t_false) = infer(f, &mut env.clone().apply(&s1), next)?;

            let s3 = t_true.clone().unify(t_false)?;
            let s_final = sc.compose(s1.compose(s2.compose(s3)));
            let t = t_true.apply(&s_final);

            Ok((s_final, t))
        }
        exp => Err(TypeError::Unimplemented(exp.clone())),
    }
}

pub fn run_infer(n: &Node) -> Result<Type, TypeError> {
    let mut ids = TId::new();
    infer(n, &mut TypeEnv(HashMap::new()), &mut ids).and_then(|(info, t)| {
        println!("{:?}", info);
        info.test_constraints().map(|_| t)
    })
}
