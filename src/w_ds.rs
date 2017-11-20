use super::desugar::{NodeDS, Pat, Arg, ModDS};
use super::parser::{self, DataDecl};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::iter;
use std::cmp::Ordering;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TId(u64);

impl TId {
    pub fn new() -> TId {
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

type Ident = Arg;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type {
    Unit,
    Int,
    Int8,
    Bool,
    Func(Box<Type>, Box<Type>),
    Closure(Box<Type>, Box<Type>, Vec<Type>),
    Tuple(Vec<Type>),
    Free(TId),
    Ptr(Box<Type>),
    Alias(String, Vec<Type>),
}

#[derive(Debug, Clone)]
pub struct EnumDecl(pub Vec<(String, Vec<Type>)>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Scheme<T: Types> {
    Type(T),
    Forall(T, Vec<(TId, HashSet<Class>)>),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Class {
    Num,
    Eq,
    Func(Type, Type),
}

#[derive(Debug, Clone)]
pub enum TypedNode {
    Lit(i64),
    Var(Arg, Type),
    Abs(Option<Arg>, Box<TypedNode>, Type, Vec<(Arg, Type)>),
    App(Box<TypedNode>, Box<TypedNode>),
    Let(String, Box<Scheme<TypedNode>>, Box<TypedNode>),
    Match(Box<TypedNode>, Vec<(Pat, TypedNode)>, Type),
    Constr(Vec<TypedNode>, Type, u64),
}

#[derive(Debug)]
pub struct TypedMod {
    pub name: String,
    pub exports: Option<Vec<String>>,
    pub types: Vec<(String, Scheme<EnumDecl>)>,
    pub defns: Vec<(String, Scheme<TypedNode>)>,
}

#[derive(Debug, Clone)]
pub struct TypeEnv {
    aliases: HashMap<String, Scheme<EnumDecl>>,
    global: HashMap<Ident, Scheme<Type>>,
    local: HashMap<Ident, Scheme<Type>>,
}

impl TypedMod {
    pub fn new(m: ModDS) -> Result<TypedMod, TypeError> {
        let ModDS {
            name,
            exports,
            types,
            defns,
        } = m;
        let types = types
            .clone()
            .into_iter()
            .map(|DataDecl {
                 name,
                 params,
                 variants,
             }| {
                (
                    name,
                    match params.len() {
                        0 => {
                            Scheme::Type(EnumDecl(
                                variants
                                    .into_iter()
                                    .map(|(i, ts)| {
                                        (
                                            i,
                                            ts.into_iter()
                                                .map(|t| Type::from(t, &HashMap::new()))
                                                .collect(),
                                        )
                                    })
                                    .collect(),
                            ))
                        }
                        n => {
                            let params = params
                                .into_iter()
                                .enumerate()
                                .map(|(i, a)| (a, TId(i as u64)))
                                .collect::<HashMap<String, TId>>();
                            Scheme::Forall(
                                EnumDecl(
                                    variants
                                        .into_iter()
                                        .map(|(i, ts)| {
                                            (
                                                i,
                                                ts.into_iter()
                                                    .map(|t| Type::from(t, &params))
                                                    .collect(),
                                            )
                                        })
                                        .collect(),
                                ),
                                params
                                    .into_iter()
                                    .map(|(_, v)| (v, HashSet::new()))
                                    .collect(),
                            )
                        }
                    },
                )
            })
            .collect::<HashMap<String, Scheme<EnumDecl>>>();
        let mut id = TId::new();
        let globals = types
            .iter()
            .flat_map(|(i, e)| match e {
                &Scheme::Type(EnumDecl(ref e)) => {
                    e.iter()
                        .map(|&(ref v, ref args)| {
                            (
                                Arg::Ident(v.clone()),
                                Scheme::Type(args.clone().into_iter().enumerate().rev().fold(
                                    Type::Alias(
                                        i.clone(),
                                        Vec::new(),
                                    ),
                                    |r, (i, a)| {
                                        match i {
                                            0 => Type::Func(Box::new(a), Box::new(r)),
                                            i => {
                                                Type::Closure(
                                                    Box::new(a),
                                                    Box::new(r),
                                                    args.iter().take(i).cloned().collect(),
                                                )
                                            }
                                        }
                                    },
                                )),
                            )
                        })
                        .collect::<Vec<_>>()
                }
                &Scheme::Forall(EnumDecl(ref e), ref params) => {
                    e.iter()
                        .map(|&(ref v, ref args)| {
                            (
                                Arg::Ident(v.clone()),
                                Scheme::Forall(
                                    args.clone().into_iter().enumerate().rev().fold(
                                        Type::Alias(
                                            i.clone(),
                                            params
                                                .iter()
                                                .map(|&(i, _)| Type::Free(i))
                                                .collect(),
                                        ),
                                        |r, (i, a)| match i {
                                            0 => Type::Func(Box::new(a), Box::new(r)),
                                            i => {
                                                Type::Closure(
                                                    Box::new(a),
                                                    Box::new(r),
                                                    args.iter().take(i).cloned().collect(),
                                                )
                                            }
                                        },
                                    ),
                                    params.clone(),
                                ),
                            )
                        })
                        .collect::<Vec<_>>()
                }
            })
            /*.chain(defns.iter().map(|&(ref name, _)| {
                (Arg::Ident(name.clone()), {
                    let id = id.next_id();
                    Scheme::Forall(Type::Free(id), vec![(id, HashSet::new())])
                })
            }))*/
            .collect();

        let mut env = TypeEnv {
            aliases: types,
            global: globals,
            local: HashMap::new(),
        };

        env.global.insert(
            Arg::Ident(String::from("llvm_add_int64")),
            Scheme::Type(Type::Func(
                Box::new(Type::Alias(
                    String::from("Tuple"),
                    vec![Type::Int, Type::Int],
                )),
                Box::new(Type::Int),
            )),
        );

        // Topo-sort to minimize out-of-order typechecking.
        let mut visited = env.global.keys().filter_map(|x| match x {
            &Arg::Ident(ref i) => Some(i.clone()),
            &Arg::Internal(_) => None,
        }).collect::<HashSet<_>>();
        let mut stack = defns.iter().map(|&(ref k, _)| k.clone()).collect::<Vec<_>>();
        let old_defns = defns.into_iter().collect::<HashMap<_,_>>();
        let mut defns = Vec::with_capacity(old_defns.len());

        while let Some(i) = stack.pop() {
            if visited.contains(&i) {
                continue;
            }
            visited.insert(i.clone());
            let n = old_defns.get(&i).ok_or_else(||TypeError::Unbound(Arg::Ident(i.clone())))?;
            stack.extend(n.free_vars().into_iter().filter_map(|a| match a {
                &Arg::Ident(ref i) => Some(i.clone()),
                &Arg::Internal(_) => None
            }));
            defns.push((String::from(i), n));
        };

        let v = Vec::with_capacity(defns.len());

        let (info, defns) = defns
            .into_iter().rev()
            .map(|(i, n)| {
                let (info, n) = infer(n, &mut env, &mut id)?;
                let s = env.generalize(n.get_type(&info), &info);
                env.global.insert(Arg::Ident(i.clone()), s);
                Ok((i, (info, n)))
            })
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .fold((TypeInfo::new(), v), |(i1, mut v), (name, (i2, n))| {
                v.push((name, n));
                (i1.compose(i2), v)
            });

        let defns = defns
            .into_iter()
            .map(|(i, n)| (i, env.generalize(n.apply(&info), &info).apply(&info)))
            .chain(env.aliases.iter().flat_map(
                |(i, e)| type_constr(i.as_str(), e),
            ))
            .collect();
        Ok(TypedMod {
            name: name,
            exports: exports,
            types: env.aliases.into_iter().collect(),
            defns: defns,
        })
    }
}

fn type_constr(name: &str, decl: &Scheme<EnumDecl>) -> Vec<(String, Scheme<TypedNode>)> {
    match *decl {
        Scheme::Type(EnumDecl(ref decl)) => {
            decl.iter()
                .enumerate()
                .map(|(j, &(ref var, ref ts))| {
                    (
                        var.clone(),
                        Scheme::Type(
                            ts.iter().enumerate().rev().fold(
                                TypedNode::Constr(
                                    ts.clone()
                                        .into_iter()
                                        .enumerate()
                                        .map(|(i, t)| TypedNode::Var(Arg::Internal(i as u64), t))
                                        .collect(),
                                    Type::Alias(String::from(name), Vec::new()),
                                    j as u64,
                                ),
                                |acc, (i, t)| {
                                    TypedNode::Abs(
                                        Some(Arg::Internal(i as u64)),
                                        Box::new(acc),
                                        t.clone(),
                                        ts.iter()
                                            .enumerate()
                                            .take(i)
                                            .map(|(i, t)| (Arg::Internal(i as u64), t.clone()))
                                            .collect(),
                                    )
                                },
                            ),
                        ),
                    )
                })
                .collect()
        }
        Scheme::Forall(EnumDecl(ref decl), ref vars) => {
            decl.iter()
                .enumerate()
                .map(|(j, &(ref var, ref ts))| {
                    (
                        var.clone(),
                        Scheme::Forall(
                            ts.iter().enumerate().rev().fold(
                                TypedNode::Constr(
                                    ts.clone()
                                        .into_iter()
                                        .enumerate()
                                        .map(|(i, t)| TypedNode::Var(Arg::Internal(i as u64), t))
                                        .collect(),
                                    Type::Alias(
                                        String::from(name),
                                        vars.iter().map(|&(id, _)| Type::Free(id)).collect(),
                                    ),
                                    j as u64,
                                ),
                                |acc, (i, t)| {
                                    TypedNode::Abs(
                                        Some(Arg::Internal(i as u64)),
                                        Box::new(acc),
                                        t.clone(),
                                        ts.iter()
                                            .enumerate()
                                            .take(i)
                                            .map(|(i, t)| (Arg::Internal(i as u64), t.clone()))
                                            .collect(),
                                    )
                                },
                            ),
                            vars.clone(),
                        ),
                    )
                })
                .collect()
        }
    }
}

impl Types for EnumDecl {
    fn ftv(&self) -> HashSet<TId> {
        self.0.iter().flat_map(|&(_, ref ts)| ts.ftv()).collect()
    }
    fn apply(self, info: &TypeInfo) -> EnumDecl {
        EnumDecl(
            self.0
                .into_iter()
                .map(|(i, ts)| (i, ts.apply(info)))
                .collect(),
        )
    }
}

impl TypedNode {
    pub fn get_type(&self, info: &TypeInfo) -> Type {
        match *self {
            TypedNode::Lit(_) => Type::Int,
            TypedNode::Var(_, ref t) => t.clone(),
            TypedNode::Abs(_, ref b, ref p, ref c) => {
                if c.len() == 0 {
                    Type::Func(Box::new(p.clone()), Box::new(b.get_type(info)))
                } else {
                    Type::Closure(
                        Box::new(p.clone()),
                        Box::new(b.get_type(info)),
                        c.iter().map(|&(_, ref t)| t.clone()).collect(),
                    )
                }
            }
            TypedNode::App(ref f, _) => f.get_ret_type(info),
            TypedNode::Let(_, _, ref b) => b.get_type(info),
            TypedNode::Match(_, _, ref b) => b.clone(),
            TypedNode::Constr(_, ref t, _) => t.clone(),
        }
    }

    pub fn get_ret_type(&self, info: &TypeInfo) -> Type {
        match self.get_type(info) {
            Type::Func(_, b) => *b,
            Type::Closure(_, b, _) => *b,
            Type::Free(id) => {
                for cls in info.constr.get(&id).expect(format!("Free type not constrained: {:?}", id).as_str()) {
                    match cls {
                        &Class::Func(_, ref r) => return r.clone(),
                        _ => {},
                    };
                }
                panic!("Free type not a function: {:?}", id)
            }
            t => panic!("Tried to get return type of non function: {:?}", t),
        }
    }
}

impl Types for TypedNode {
    fn ftv(&self) -> HashSet<TId> {
        match *self {
            TypedNode::Lit(_) => HashSet::new(),
            TypedNode::Var(_, ref t) => t.ftv(),
            TypedNode::Abs(_, ref b, ref p, ref c) => {
                &(&b.ftv() | &p.ftv()) | &c.iter().flat_map(|&(_, ref t)| t.ftv()).collect()
            }
            TypedNode::App(ref f, ref p) => &f.ftv() | &p.ftv(),
            TypedNode::Let(_, ref b, ref r) => &b.ftv() | &r.ftv(),
            TypedNode::Match(ref a, _, ref t) => &a.ftv() | &t.ftv(),
            TypedNode::Constr(ref a, ref t, _) => &a.ftv() | &t.ftv(),
        }
    }

    fn apply(self, info: &TypeInfo) -> TypedNode {
        match self {
            TypedNode::Lit(l) => TypedNode::Lit(l),
            TypedNode::Var(i, t) => TypedNode::Var(i, t.apply(info)),
            TypedNode::Abs(a, b, p, c) => {
                TypedNode::Abs(
                    a,
                    Box::new(b.apply(info)),
                    p.apply(info),
                    c.into_iter().map(|(i, t)| (i, t.apply(info))).collect(),
                )
            }
            TypedNode::App(f, p) => {
                TypedNode::App(Box::new(f.apply(info)), Box::new(p.apply(info)))
            }
            TypedNode::Let(i, t, b) => {
                TypedNode::Let(i, Box::new(t.apply(info)), Box::new(b.apply(info)))
            }
            TypedNode::Match(a, arms, t) => {
                TypedNode::Match(
                    Box::new(a.apply(info)),
                    arms.into_iter().map(|(p, n)| (p, n.apply(info))).collect(),
                    t.apply(info),
                )
            }
            TypedNode::Constr(a, t, i) => TypedNode::Constr(a.apply(info), t.apply(info), i),
        }
    }
}

// type Scheme = (Vec<TId>, Type);

pub trait Types {
    fn ftv(&self) -> HashSet<TId>;
    fn apply(self, &TypeInfo) -> Self;
}

impl Types for Type {
    fn ftv(&self) -> HashSet<TId> {
        match self {
            &Type::Free(id) => iter::once(id).collect(),
            &Type::Func(ref p, ref r) => &p.ftv() | &r.ftv(),
            &Type::Closure(ref p, ref r, ref hs) => &(&p.ftv() | &r.ftv()) | &hs.ftv(),
            &Type::Tuple(ref ts) => ts.iter().fold(HashSet::new(), |s, t| &s | &t.ftv()),
            &Type::Alias(_, ref ts) => ts.iter().flat_map(Types::ftv).collect(),
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
            Type::Tuple(ts) => Type::Tuple(ts.into_iter().map(|t| t.apply(s)).collect()),
            Type::Alias(i, ts) => Type::Alias(i, ts.into_iter().map(|t| t.apply(s)).collect()),
            t => t,
        }
    }
}

impl<T: Types> Types for Scheme<T> {
    fn ftv(&self) -> HashSet<TId> {
        match self {
            &Scheme::Type(ref t) => t.ftv(),
            &Scheme::Forall(ref t, ref v) => {
                &t.ftv() - &v.iter().map(|&(var, _)| var.clone()).collect()
            } // TODO How to use the constraignts.
        }
    }

    fn apply(self, s: &TypeInfo) -> Scheme<T> {
        match self {
            Scheme::Type(t) => Scheme::Type(t.apply(s)),
            Scheme::Forall(t, v) => {
                //let mut s = s.clone();
                //v.iter().map(|&(id, _)| { s.remove(id); }).last();
                Scheme::Forall(
                    t.apply(&s),
                    v.into_iter().map(|(id, c)| (id, c.apply(&s))).collect(),
                )
            }
        }
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
where
    T: Types,
{
    fn ftv(&self) -> HashSet<TId> {
        self.iter().map(Types::ftv).fold(
            HashSet::new(),
            |a, b| &a | &b,
        )
    }
    fn apply(self, s: &TypeInfo) -> HashSet<T> {
        self.into_iter().map(|t| t.apply(s)).collect()
    }
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
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

impl TypeEnv {
    fn new() -> TypeEnv {
        TypeEnv {
            aliases: HashMap::new(),
            global: HashMap::new(),
            local: HashMap::new(),
        }
    }
    fn generalize<T: Types>(&self, t: T, info: &TypeInfo) -> Scheme<T> {
        let vars = &t.ftv() - &self.ftv();
        let mut new_vars = HashSet::new();

        let mut stack = vars.into_iter().collect::<Vec<TId>>();
        while let Some(id) = stack.pop() {
            if !new_vars.contains(&id) {
                new_vars.insert(id);
                match info.constr.get(&id) {
                    Some(c) => stack.extend(c.iter().flat_map(Types::ftv)),
                    None => {},
                };
            }
        }

        match new_vars.len() {
            0 => Scheme::Type(t),
            n => {
                Scheme::Forall(
                    t,
                    new_vars.into_iter()
                        .map(|id| {
                            let cls = info.constr
                                .get(&id)
                                .into_iter()
                                .flat_map(|cls| cls.clone())
                                .collect();
                            (id, cls)
                        })
                        .collect(),
                )
            }
        }
    }

    fn extend(&self, other: TypeEnv) -> TypeEnv {
        TypeEnv {
            global: self.global
                .clone()
                .into_iter()
                .chain(other.global)
                .collect(),
            local: self.local.clone().into_iter().chain(other.local).collect(),
            aliases: self.aliases
                .clone()
                .into_iter()
                .chain(other.aliases)
                .collect(),
        }
    }
}

impl Types for TypeEnv {
    fn ftv(&self) -> HashSet<TId> {
        self.global
            .values()
            .chain(self.local.values())
            .flat_map(Types::ftv)
            .collect()
    }
    fn apply(self, s: &TypeInfo) -> TypeEnv {
        TypeEnv {
            global: self.global
                .into_iter()
                .map(|(i, t)| (i, t.apply(s)))
                .collect(),
            local: self.local
                .into_iter()
                .map(|(i, t)| (i, t.apply(s)))
                .collect(),
            aliases: self.aliases,
        }
    }
}

// struct Constraints(HashMap<TId, HashSet<Class>>);

impl TypeInfo {
    pub fn new() -> TypeInfo {
        TypeInfo {
            subst: HashMap::new(),
            constr: HashMap::new(),
        }
    }

    pub fn compose(self, other: TypeInfo) -> TypeInfo {
        let TypeInfo {
            subst: s,
            constr: c,
        } = other;
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
        constr.into_iter().fold(Ok(self), |info, (id, cls)| {
            match subst.get(&id) {
                Some(t) => {
                    cls.clone().into_iter().fold(info, |info, cl| {
                        info.and_then(|info| {
                            t.clone().unify_class(cl.apply(&info)).map(
                                |i| i.compose(info),
                            )
                        })
                    })
                }
                None => info,
            }
        })
    }

    fn remove(&mut self, id: TId) {
        self.subst.remove(&id);
        self.constr.remove(&id);
    }

    fn test_constraints(&self) -> Result<(), TypeError> {
        let &TypeInfo {
            ref subst,
            ref constr,
        } = self;
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

impl<T: Types + Clone> Scheme<T> {
    fn instantiate(&self, next: &mut TId) -> (TypeInfo, T) {
        match self {
            &Scheme::Type(ref t) => (TypeInfo::new(), t.clone()),
            &Scheme::Forall(ref t, ref v) => {
                // println!("{:?} {{{:?}}}", t, v);
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
}
/*
impl Scheme<Type> {
    fn unify(self, other: Scheme<Type>) -> Result<TypeInfo, TypeError> {
        match (self, other) {
            (Scheme::Type(t1), Scheme::Type(t2)) => t1.unify(t2),
            (Scheme::Type(t1), Scheme::Forall(t2, cls)) | (Scheme::Forall(t2, cls), Scheme::Type(t1)) => {
                let i = t1.clone().unify(t2);
                let t = t1.apply(&i);
                let cls = t.ftv().into_iter().map(|id| (id, cls.get(id).into_iter().flat_map(|x|x).collect()).collect();
                match cls.len() {
                    0 => Scheme::Type(t),
                    _ => Scheme::Forall(t, cls),
                }
            },
            (Scheme::Forall(t1, c1), Scheme::Forall(t2, c2)) => {
                let i = t1.clone().unify(t2);
                let t = t1.apply(&i);
                let cls = t.ftv().into_iter().map(|id| (id, c1.get(id).into_iter().chain(c2.get(id)).flat_map(|x|x).collect()).collect();
                match cls.len() {
                    0 => Scheme::Type(t),
                    _ => Scheme::Forall(t, cls),
                }
            }
        }
    }
}
*/
impl Scheme<TypedNode> {
    pub fn get_type(&self, info: &TypeInfo) -> Scheme<Type> {
        match self {
            &Scheme::Type(ref t) => Scheme::Type(t.get_type(info)),
            &Scheme::Forall(ref t, ref ids) => {
                let mut i = info.clone();
                for &(ref id, ref cls) in ids {
                    i.constr.entry(*id).or_insert(HashSet::with_capacity(cls.len())).extend(cls.clone());
                }
                Scheme::Forall(t.get_type(&i), ids.clone())
            }
        }
    }
}

impl<T: Types> Scheme<T> {
    fn map<V: Types, F: Fn(T) -> V>(self, f: F) -> Scheme<V> {
        match self {
            Scheme::Type(t) => Scheme::Type(f(t)),
            Scheme::Forall(t, c) => Scheme::Forall(f(t), c),
        }
    }

    fn map_r<V: Types, F: Fn(&T) -> V>(&self, f: F) -> Scheme<V> {
        match *self {
            Scheme::Type(ref t) => Scheme::Type(f(t)),
            Scheme::Forall(ref t, ref c) => Scheme::Forall(f(t), c.clone()),
        }
    }

    fn flat_map_r<I: IntoIterator, F: Fn(&T) -> I>(&self, f: F) -> Vec<Scheme<I::Item>>
    where
        I::Item: Types,
    {
        match *self {
            Scheme::Type(ref t) => f(t).into_iter().map(Scheme::Type).collect(),
            Scheme::Forall(ref t, ref c) => {
                f(t)
                    .into_iter()
                    .map(|t| Scheme::Forall(t, c.clone()))
                    .collect()
            }
        }
    }
}

#[derive(Debug)]
pub enum TypeError {
    Unbound(Ident),
    NoUnify(Type, Type),
    Recursive(Type, TId),
    Wrapped(NodeDS, Box<TypeError>),
    Unimplemented(NodeDS),
    Constrained(Type, Class),
    Other,
}

impl Type {
    pub fn unify(self, other: Type) -> Result<TypeInfo, TypeError> {
        match (self, other) {
            (Type::Func(l1, r1), Type::Func(l2, r2)) => {
                let s1 = l1.unify(*l2)?;
                let s2 = Type::unify(r1.apply(&s1), r2.apply(&s1))?;
                Ok(s1.compose(s2))
            }
            (Type::Closure(p1, r1, h1), Type::Closure(p2, r2, h2)) => {
                if h1.len() != h2.len() {
                    return Err(TypeError::NoUnify(
                        Type::Closure(p1, r1, h1),
                        Type::Closure(p2, r2, h2),
                    ));
                }
                let s1 = p1.unify(*p2)?;
                let s2 = Type::unify(r1.apply(&s1), r2.apply(&s1))?;

                h1.into_iter().zip(h2.into_iter()).fold(
                    Ok(s1.compose(s2)),
                    |s, (t1, t2)| {
                        s.and_then(|s| Ok(Type::unify(t1.apply(&s), t2.apply(&s))?.compose(s)))
                    },
                )
            }
            (Type::Tuple(ts1), Type::Tuple(ts2)) => {
                ts1.into_iter().zip(ts2).fold(
                    Ok(TypeInfo::new()),
                    |i, (t1, t2)| {
                        i.and_then(|i| Ok(Type::unify(t1.apply(&i), t2.apply(&i))?.compose(i)))
                    },
                )
            }
            (Type::Alias(i1, ts1), Type::Alias(i2, ts2)) => {
                if i1 != i2 {
                    Err(TypeError::NoUnify(
                        Type::Alias(i1, ts1),
                        Type::Alias(i2, ts2),
                    ))
                } else {
                    ts1.into_iter().zip(ts2).fold(
                        Ok(TypeInfo::new()),
                        |i, (t1, t2)| {
                            i.and_then(|i| Ok(Type::unify(t1.apply(&i), t2.apply(&i))?.compose(i)))
                        },
                    )
                }
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

    fn unify_pat(self, p: Pat, next: &mut TId) -> Result<(TypeInfo, TypeEnv), TypeError> {
        match (self, p) {
            (t, Pat::Prim(i)) => {
                Ok((
                    TypeInfo::new(),
                    TypeEnv {
                        global: HashMap::new(),
                        local: i.map(|i| (i, Scheme::Type(t))).into_iter().collect(),
                        aliases: HashMap::new(),
                    },
                ))
            }
            // (Type::Int, Pat::Lit(_)) => Ok((TypeInfo::new() TypeEnv::new())),
            (t, Pat::Lit(_)) => {
                t.clone().unify(Type::Int).map(
                    |info| (info, TypeEnv::new()),
                )
            }
            (t, Pat::Cons(c, args)) => {
                if c.as_str() == "True" || c.as_str() == "False" {
                    t.unify(Type::Bool).map(|info| (info, TypeEnv::new()))
                } else if c.as_str() == "()" {
                    t.unify(Type::Unit).map(|info| (info, TypeEnv::new()))
                } else if c.chars().all(|ch| ch == ',') {
                    let test = Type::Tuple(c.chars().map(|_| next.next_t()).collect());
                    let info = t.clone().unify(test)?;
                    if let Type::Tuple(ts) = t.apply(&info) {
                        let env = TypeEnv {
                            global: HashMap::new(),
                            local: args.into_iter()
                                .zip(ts.iter())
                                .flat_map(|(p, t)| p.map(|p| (p, Scheme::Type(t.clone()))))
                                .collect(),
                            aliases: HashMap::new(),
                        };
                        Ok((info, env))
                    } else {
                        unreachable!()
                    }
                } else {
                    Err(TypeError::Unbound(Arg::Ident(c.clone())))
                }
            }
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
            &Type::Tuple(ref ts) => ts.iter().any(|t| t.contains(id)),
            _ => false,
        }
    }

    fn in_class(&self, c: &Class) -> bool {
        match (self, c) {
            (&Type::Free(_), _) => true,
            (&Type::Int, &Class::Num) |
            (&Type::Int8, &Class::Num) => true,
            (&Type::Int, &Class::Eq) |
            (&Type::Bool, &Class::Eq) |
            (&Type::Unit, &Class::Eq) |
            (&Type::Int8, &Class::Eq) => true,
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

    fn from(t: parser::Type, ids: &HashMap<String, TId>) -> Type {
        match t {
            parser::Type::Unit => Type::Unit,
            parser::Type::Int => Type::Int,
            parser::Type::Int8 => Type::Int8,
            parser::Type::Bool => Type::Bool,
            parser::Type::Func(a, r) => {
                Type::Func(Box::new(Type::from(*a, ids)), Box::new(Type::from(*r, ids)))
            }
            parser::Type::Tuple(ts) => {
                Type::Tuple(ts.into_iter().map(|t| Type::from(t, ids)).collect())
            }
            parser::Type::Param(i) => Type::Free(*ids.get(&i).expect("Undeclared type id.")),
            t => panic!("Unimplemented: {:?}", t),
        }
    }
}

fn infer_pat(p: &Pat, next: &mut TId) -> Result<Type, TypeError> {
    match p {
        &Pat::Prim(_) => Ok(next.next_t()),
        &Pat::Lit(_) => Ok(Type::Int),
        &Pat::Cons(ref c, ref args) => {
            if c.as_str() == "True" || c.as_str() == "False" {
                if args.len() == 0 {
                    Ok(Type::Bool)
                } else {
                    // println!("{:?}", args);
                    Err(TypeError::Other)
                }
            } else if c.as_str() == "()" {
                if args.len() == 0 {
                    Ok(Type::Unit)
                } else {
                    Err(TypeError::Other)
                }
            } else if c.chars().all(|ch| ch == ',') {
                if args.len() == c.len() + 1 {
                    Ok(Type::Tuple(args.iter().map(|_| next.next_t()).collect()))
                } else {
                    Err(TypeError::Other)
                }
            } else {
                Err(TypeError::Unbound(Arg::Ident(c.clone())))
            }
        }
    }
}

pub fn infer(
    n: &NodeDS,
    env: &mut TypeEnv,
    next: &mut TId,
) -> Result<(TypeInfo, TypedNode), TypeError> {
    match n {
        &NodeDS::Var(ref i) => {
            env.local
                .get(i)
                .or_else(|| env.global.get(i))
                .map(|s| s.instantiate(next))
                .map(|(info, t)| (info, TypedNode::Var(i.clone(), t)))
                .ok_or(TypeError::Unbound(i.clone()))
        }
        &NodeDS::Lit(i) => Ok((TypeInfo::new(), TypedNode::Lit(i))),
        &NodeDS::Abs(ref i, ref e) => {
            let new_id = next.next_t();
            // let env = env.remove(i.as_str());
            let mut env = match i {
                &Some(ref i) => {
                    let mut e = env.clone();
                    e.local.insert(i.clone(), Scheme::Type(new_id.clone()));
                    e
                }
                &None => env.clone(),
            };

            let (s, body) = infer(e, &mut env, next)?;

            let mut free = e.free_vars();
            match i {
                &Some(ref i) => {
                    free.remove(i);
                }
                &None => {}
            };
            let enumed = free.into_iter()
                .filter_map(|i| match (env.local.get(i), env.global.get(i)) {
                    (Some(s1), _) => Some(Ok((i.clone(), s1.instantiate(next)))),
                    (None, Some(_)) => None,
                    (None, None) => Some(Err(TypeError::Unbound(i.clone()))),
                })
                .collect::<Result<Vec<_>, _>>()?;
            if enumed.is_empty() {
                // let t = Type::Func(Box::new()new_id.apply(&s), Box::new(t));
                let nt = TypedNode::Abs(i.clone(), Box::new(body), new_id, Vec::new()).apply(&s);
                Ok((s, nt))
            } else {
                let (idents, schemes): (Vec<_>, Vec<_>) = enumed.into_iter().unzip();
                let (infos, ts): (Vec<_>, Vec<_>) = schemes.into_iter().unzip();
                let info = infos.into_iter().fold(s, |i1, i2| i1.compose(i2));
                let ts = idents
                    .into_iter()
                    .zip(ts.into_iter().map(|t| t.apply(&info)))
                    .collect();
                // let t = Type::Closure(Box::new(new_id.apply(&info)), Box::new(t), ts);
                let tagged = TypedNode::Abs(i.clone(), Box::new(body), new_id, ts).apply(&info);

                Ok((info, tagged))
            }
        }
        &NodeDS::App(ref f, ref a) => {
            let err = |e| TypeError::Wrapped(NodeDS::App(f.clone(), a.clone()), Box::new(e));
            (|f, a| {
                let t = next.next_t();

                let (s1, tf) = infer(f, env, next)?;
                let (s2, ta) = infer(a, &mut env.clone().apply(&s1), next)?;

                let s = s1.compose(s2);

                let sc = tf.get_type(&s).unify_class(
                    Class::Func(ta.get_type(&s), t.clone()),
                )?;
                let s = s.compose(sc).apply_constraints()?;

                let tagged = TypedNode::App(Box::new(tf), Box::new(ta));

                let t = tagged.apply(&s);
                //eprintln!("{:?}", s);
                Ok((s, t))
            })(f, a).map_err(err)
        }
        &NodeDS::Let(ref i, ref b, ref e) => {
            let mut env = env.clone();

            // let tmp = next.next_t();
            // env.0.insert(Arg::Ident(i.clone()), Scheme::Type(tmp.clone()));

            let (s1, tb) = infer(b, &mut env, next)?;
            // println!("{:?} ({:?})", t1, s1);
            // let sb = s1.compose(tmp.unify(t1.clone())?);
            let sb = s1;
            let t = env.clone().apply(&sb).generalize(tb, &sb);

            env.local.insert(Arg::Ident(i.clone()), t.get_type(&sb));

            env = env.apply(&sb);

            let (s2, tn) = infer(e, &mut env, next)?;

            let info = sb.compose(s2).apply_constraints()?;

            let tagged = TypedNode::Let(i.clone(), Box::new(t), Box::new(tn)).apply(&info);

            Ok((info, tagged))
        }
        &NodeDS::Match(ref e, ref arms) => {
            let (sa, ta) = infer(e, env, next)?;
            let (s, t) = arms.iter()
                .map(|&(ref p, _)| infer_pat(p, next))
                .collect::<Result<Vec<_>, TypeError>>()?
                .into_iter()
                .fold(Ok((sa, ta)), |r, p| {
                    r.and_then(|(s, t)| {
                        let ty = t.get_type(&s).unify(p)?;
                        let s = s.compose(ty);
                        let t = t.apply(&s);
                        Ok((s, t))
                    })
                })?;

            let (infos, envs): (Vec<_>, Vec<_>) = arms.iter()
                .map(|&(ref p, _)| t.get_type(&s).unify_pat(p.clone(), next))
                .collect::<Result<Vec<(_, _)>, _>>()?
                .into_iter()
                .unzip();
            let info = infos.into_iter().fold(s, |i1, i2| i1.compose(i2));
            let arm_ts: Vec<_> = arms.iter()
                .map(|&(_, ref b)| b)
                .zip(envs)
                .map(|(b, e)| infer(b, &mut env.extend(e), next))
                .collect::<Result<Vec<_>, _>>()?;
            // println!("{:?}", t);

            // arm_ts.into_iter()
            // .fold(Ok((info, next.next_t())), |r, (i, ty)| {
            // r.and_then(|(info, t)| {
            // let i = info.compose(i).compose(t.clone().unify(ty)?);
            // let t = t.apply(&i);
            // Ok((i, t))
            // })
            // })
            //

            let (info, t_arm) = arm_ts.iter().fold(Ok((info, next.next_t())), |r,
             &(ref i, ref n)| {
                r.and_then(|(info, t)| {
                    let i = info.compose(i.clone());
                    let ty = n.get_type(&i);
                    let i = i.compose(t.clone().unify(ty)?);
                    let t = t.apply(&i);
                    Ok((i, t))
                })
            })?;
            let arms = arms.clone()
                .into_iter()
                .map(|(p, _)| p)
                .zip(arm_ts.into_iter().map(|(_, n)| n))
                .collect();
            let n = TypedNode::Match(Box::new(t), arms, t_arm).apply(&info);
            Ok((info, n))
        }
    }
}

fn build_tup_ty(i: u64) -> (Ident, Scheme<Type>) {
    let id = Arg::Ident(iter::repeat(',').take(i as usize - 1).collect());
    let t = (0..i).rev().fold(
        Type::Tuple((0..i).map(|i| Type::Free(TId(i))).collect()),
        |t, j| {
            Type::Closure(
                Box::new(Type::Free(TId(j))),
                Box::new(t),
                (j..i).map(|i| Type::Free(TId(i))).collect(),
            )
        },
    );
    (
        id,
        Scheme::Forall(t, (0..i).map(|i| (TId(i), HashSet::new())).collect()),
    )
}
// pub fn run_infer(n: &NodeDS) -> Result<TypedNode, TypeError> {
// let mut ids = TId::new();
//
// let add_id = ids.next_id();
// let add_ty = Type::Free(add_id);
//
// let eq_id = ids.next_id();
// let eq_ty = Type::Free(eq_id);
//
// let env = vec![
// (
// Arg::Ident(String::from("+")),
// Scheme::Forall(
// Type::Func(
// Box::new(add_ty.clone()),
// Box::new(Type::Closure(
// Box::new(add_ty.clone()),
// Box::new(add_ty.clone()),
// vec![add_ty.clone()],
// )),
// ),
// vec![(add_id, iter::once(Class::Num).collect())],
// )
// ),
// (
// Arg::Ident(String::from("==")),
// Scheme::Forall(
// Type::Func(
// Box::new(eq_ty.clone()),
// Box::new(Type::Closure(
// Box::new(eq_ty.clone()),
// Box::new(Type::Bool),
// vec![eq_ty.clone()],
// )),
// ),
// vec![(eq_id, iter::once(Class::Eq).collect())],
// )
// ),
// (Arg::Ident(String::from("()")), Scheme::Type(Type::Unit)),
// (Arg::Ident(String::from("True")), Scheme::Type(Type::Bool)),
// (Arg::Ident(String::from("False")), Scheme::Type(Type::Bool)),
// build_tup_ty(2),
// build_tup_ty(3),
// ].into_iter()
// .collect();
//
// infer(n, &mut TypeEnv(env), &mut ids).and_then(|(info, t)| {
// eprintln!("{:?}", info);
// info.test_constraints().map(|_| t)
// })
// }
//
