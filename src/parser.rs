use std::str::{self, FromStr};
use std::iter::FromIterator;
use nom::{IResult, IError, digit, alphanumeric, anychar, line_ending};

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    Lit(Literal),
    // BinOp(Op, Box<Node>, Box<Node>),
    Op(Op),
    FunCall(Box<Node>, Box<Node>),
    Ident(String),
    Let(Vec<(Pattern, Node)>, Box<Node>),
    If(Box<Node>, Box<Node>, Box<Node>),
    Tuple(Vec<Node>),
    Lambda(Pattern, Option<Type>, Box<Node>),
    Match(Box<Node>, Vec<(Pattern, Node)>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Ident(String),
    Lit(Literal),
    Constructor(String, Vec<Pattern>),
    Tuple(Vec<Pattern>),
    Wild,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Unit,
    True,
    False,
    Int(i64),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Unit,
    Int,
    Bool,
    Func(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Box(Box<Type>),
    Ptr(Box<Type>),
    Param(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Equal,
}

#[derive(Debug, PartialEq)]
pub struct Defn {
    pub name: String,
    pub args: Vec<Pattern>,
    pub val: Node,
}

#[derive(Debug, PartialEq)]
pub struct DataDecl {
    name: String,
    params: Vec<String>,
    variants: Vec<(String, Vec<Type>)>,
}

#[derive(Debug, PartialEq)]
pub enum Decl {
    Defn(Defn),
    Type(String, Type),
    Data(DataDecl),
}

#[derive(Debug, PartialEq)]
pub struct Module {
    pub name: String,
    pub exports: Option<Vec<String>>,
    pub decls: Vec<Decl>,
}

impl Op {
    fn prec(self) -> i64 {
        use self::Op::*;
        match self {
            Add | Sub => 2,
            Mul | Div => 3,
            Equal => 1,
        }
    }
}

impl Node {
    pub fn new(src: &str) -> Result<Node, IError> {
        expr(src.as_bytes()).to_full_result()
    }
}

impl Type {
    pub fn new(src: &str) -> Result<Type, IError> {
        type_expr(src.as_bytes()).to_full_result()
    }
}

impl Module {
    pub fn new(src: &[u8]) -> Result<Module, IError> {
        module(src).to_full_result()
    }
}

named!(pub space, eat_separator!(&b" \t"[..]));
macro_rules! ws_nl (
  ($i:expr, $($args:tt)*) => (
    {
// use $crate::space;
      sep!($i, space, $($args)*)
    }
  )
);

fn parse_bin_expr(src: &[u8], prec: i64) -> IResult<&[u8], Node> {
    do_parse!(src,
		left: atom >>
		i: ws_nl!(apply!(parse_bin_expr_full, prec, left)) >>
		(i)
	)
}

fn parse_bin_expr_full(src: &[u8], prec: i64, lhs: Node) -> IResult<&[u8], Node> {
    if src.len() == 0 {
        return IResult::Done(src, lhs);
    }

    let (i1, tok) = match binop(src) {
        IResult::Done(i1, tok) => (i1, tok),
        _ => return IResult::Done(src, lhs),
    };
    if tok.prec() > prec {
        let (i2, right) = try_parse!(i1, ws_nl!(apply!(parse_bin_expr, tok.prec())));
        parse_bin_expr_full(
            i2,
            prec,
            Node::FunCall(
                Box::new(Node::FunCall(Box::new(Node::Op(tok)), Box::new(lhs))),
                Box::new(right),
            ),
        )
    } else {
        IResult::Done(src, lhs)
    }
}

named!(keyword<&str>, map_res!(alt_complete!(
    tag!("let") | tag!("in") | tag!("if") | tag!("then") | tag!("else") | tag!("case") | tag!("of")
), str::from_utf8));
named!(builtin_type<Type>, switch!(alphanumeric,
	b"Int" => value!(Type::Int) |
	b"Bool" => value!(Type::Bool) |
    b"Box" => map!(ws_nl!(type_atom), |t| Type::Box(Box::new(t))) |
    b"Ptr" => map!(ws_nl!(type_atom), |t| Type::Ptr(Box::new(t)))
));
named!(bool_lit<Literal>, switch!(alphanumeric,
	b"True" => value!(Literal::True) |
	b"False" => value!(Literal::False)
));

named!(reserved<()>, alt_complete!(
    value!((), keyword) | value!((), builtin_type) | value!((), bool_lit)
));

named!(int<Literal>, map!(map_res!(map_res!(digit, str::from_utf8),
    FromStr::from_str), Literal::Int));
named!(literal<Literal>, alt_complete!(int | bool_lit));

named!(ident_char<char>, verify!(anychar, |ch: char| ch.is_alphanumeric() || ch == '_'));

named!(upper_ident<String>, do_parse!(
    not!(reserved) >>
    first: peek!(anychar) >>
    i: cond_reduce!(first.is_uppercase(), map!(many1!(ident_char), String::from_iter)) >>
    (i)
));

named!(lower_ident<String>, do_parse!(
    not!(reserved) >>
    first: peek!(anychar) >>
    i: cond_reduce!(first.is_lowercase(), map!(many1!(ident_char), String::from_iter)) >>
    (i)
));

named!(ident<String>, alt_complete!(upper_ident | lower_ident));

named!(paren<Node>, delimited!( tag!("("), map!(ws!(
  separated_list!(tag!(","), expr)), |mut es| match es.len() {
	0 => Node::Lit(Literal::Unit),
	1 => es.pop().unwrap(),
	_ => Node::Tuple(es),
}), tag!(")")));


named!(atom<Node>, alt_complete!(
    map!(literal, Node::Lit) | map!(ident, Node::Ident) | op_fun | paren));

named!(addop<Op>, value!(Op::Add, tag!("+")));
named!(subop<Op>, value!(Op::Sub, tag!("-")));
named!(mulop<Op>, value!(Op::Mul, tag!("*")));
named!(divop<Op>, value!(Op::Div, tag!("/")));
named!(eqop<Op>, value!(Op::Equal, tag!("==")));

named!(binop<Op>, alt_complete!(addop | subop | mulop | divop | eqop));
named!(op_fun<Node>, delimited!( tag!("("), map!(binop, Node::Op), tag!(")")));

named!(funcall<Node>, fold_many1!( ws_nl!(apply!(parse_bin_expr, 0)),
  Node::Lit(Literal::Unit), |acc, n| match acc {
	Node::Lit(Literal::Unit) => n,
	acc => Node::FunCall(Box::new(acc), Box::new(n)),
}));


named!(tuple_pattern<Pattern>, map!(ws_nl!(delimited!(
	tag!("("),
	separated_list!(tag!(","), pattern),
	tag!(")")
)), |mut ps| match ps.len() {
	0 => Pattern::Lit(Literal::Unit),
	1 => ps.pop().unwrap(),
	_ => Pattern::Tuple(ps),
}));

named!(constr_pattern<Pattern>, map!(
    ws_nl!(alt_complete!(tuple!(upper_ident, many0!(pattern_atom)) |
                         tuple!(lower_ident, many1!(pattern_atom)))),
    |(c, v)| Pattern::Constructor(c, v)
));

named!(pattern_atom<Pattern>, alt_complete!(
    map!(tag!("_"), |_| Pattern::Wild) | map!(literal, Pattern::Lit) |
    map!(lower_ident, Pattern::Ident) | tuple_pattern));
named!(pattern<Pattern>, alt_complete!(constr_pattern | pattern_atom));

named!(let_binding<(Pattern, Node)>, ws_nl!(do_parse!(
	p: pattern >>
	tag!("=") >>
	v: expr >>
	(p, v)
)));

named!(let_expr<Node>, do_parse!(
	ws!(tag!("let")) >>
	b: separated_list!(ws_nl!(eat_separator!("\n;")), let_binding) >>
	ws!(tag!("in")) >>
	e: expr >>
	(Node::Let(b, Box::new(e)))
));

named!(if_expr<Node>, ws!(do_parse!(
	tag!("if") >>
	c: expr >>
	tag!("then") >>
	t: expr >>
	tag!("else") >>
	f: expr >>
	(Node::If(Box::new(c), Box::new(t), Box::new(f)))
)));

named!(case_expr<Node>, ws_nl!(do_parse!(
    tag!("case") >>
    arg: expr >>
    tag!("of") >>
    cases: separated_list!(ws_nl!(eat_separator!("\n;")), do_parse!(
        p: pattern >>
        tag!("->") >>
        val: expr >>
        ((p, val))
    )) >>
    (Node::Match(Box::new(arg), cases))
)));

named!(lamdba<Node>, ws_nl!(do_parse!(
	tag!("\\") >>
	arg: pattern_atom >>
    ty: opt!(ws_nl!(do_parse!(tag!(":") >> ty: type_atom >> (ty)))) >>
	tag!("->") >>
	val: expr >>
	(Node::Lambda(arg, ty, Box::new(val)))
)));

named!(expr<Node>, alt_complete!(funcall | let_expr | if_expr | lamdba | case_expr));

named!(tuple_type<Type>, delimited!( tag!("("), map!(ws!(
  separated_list!(tag!(","), type_expr)), |mut ts| match ts.len() {
	0 => Type::Unit,
	1 => ts.pop().unwrap(),
	_ => Type::Tuple(ts),
}), tag!(")")));

named!(func_type<Type>, map!(
    ws_nl!(tuple!(type_atom, tag!("->"), type_expr)),
    |(param, _, ret)| Type::Func(Box::new(param), Box::new(ret))
));

named!(type_atom<Type>, alt_complete!(builtin_type | map!(lower_ident, Type::Param) | tuple_type));
named!(type_expr<Type>, alt_complete!(func_type | type_atom));

named!(decl<(String, Type)>, ws_nl!(do_parse!(
	i: lower_ident >>
	tag!(":") >>
	t: type_expr >>
	(i, t)
)));

named!(defn<Defn>, ws_nl!(do_parse!(
	i: lower_ident >>
	a: ws_nl!(many0!(pattern_atom)) >>
	tag!("=") >>
	e: expr >>
	(Defn{name: i, args: a, val: e})
)));

named!(data_decl<DataDecl>, ws_nl!(do_parse!(
    tag!("data") >>
    name: upper_ident >>
    params: ws_nl!(many0!(lower_ident)) >>
    tag!("=") >>
    vars: ws!(separated_nonempty_list_complete!(tag!("|"),
              ws_nl!(tuple!(upper_ident, many0!(type_atom))))) >>
    (DataDecl{name: name, params: params, variants: vars})
)));

named!(top_level<Vec<Decl>>, separated_nonempty_list_complete!(line_ending, alt_complete!(
    map!(data_decl, Decl::Data) | map!(decl, |(i,t)| Decl::Type(i, t)) | map!(defn, Decl::Defn)
)));

named!(module<Module>, ws_nl!(do_parse!(
    tag!("module") >>
    name: upper_ident >>
    exports: ws!(delimited!(tag!("("), alt_complete!(
        value!(None, tag!("..")) |
        map!(separated_list_complete!(tag!(","), ident), Some)) ,tag!(")"))) >>
    decls: top_level >>
    (Module{name: name.to_owned(), exports: exports, decls: decls})
)));

pub fn test_lambda() {
    assert_eq!(expr(b"(\\x : Int -> x)"), IResult::Done([].as_ref(),
        Node::Lambda(Pattern::Ident("x".into()),
            Some(Type::Int), Box::new(Node::Ident("x".into())))));
}

pub fn test_exprs() {
    assert_eq!(expr(b"True"), IResult::Done(&[] as &[u8], Node::Lit(Literal::True)));
    assert_eq!(expr(b"False"), IResult::Done(&[] as &[u8], Node::Lit(Literal::False)));
    assert_eq!(expr(b"iszero e"), IResult::Done(&[] as &[u8], Node::FunCall(
        Box::new(Node::Ident("iszero".to_owned())), Box::new(Node::Ident("e".to_owned())))));
    assert_eq!(expr(b"succ e"), IResult::Done(&[] as &[u8], Node::FunCall(
        Box::new(Node::Ident("succ".to_owned())), Box::new(Node::Ident("e".to_owned())))));
    assert_eq!(expr(b"pred e"), IResult::Done(&[] as &[u8], Node::FunCall(
        Box::new(Node::Ident("pred".to_owned())), Box::new(Node::Ident("e".to_owned())))));
    assert_eq!(expr(b"if e then e else e"), IResult::Done(&[] as &[u8], Node::If(
        Box::new(Node::Ident("e".to_owned())),
        Box::new(Node::Ident("e".to_owned())),
        Box::new(Node::Ident("e".to_owned())))));
    assert_eq!(expr(b"0"), IResult::Done(&[] as &[u8], Node::Lit(Literal::Int(0))));
    eprintln!("");
    let expected = IResult::Done(&[] as &[u8], Type::Int);
    assert_eq!(type_expr(b"Int"), expected);
    let expected = IResult::Done(&[] as &[u8], Type::Bool);
    assert_eq!(type_expr(b"Bool"), expected);
    let expected = IResult::Done(&[] as &[u8], Type::Tuple(vec![Type::Int, Type::Int]));
    assert_eq!(type_expr(b"(Int, Int)"), expected);
    let expected = IResult::Done(
        &[] as &[u8],
        Type::Func(
            Box::new(Type::Int),
            Box::new(Type::Func(Box::new(Type::Int), Box::new(Type::Bool))),
        ),
    );
    assert_eq!(type_expr(b"(Int -> Int -> Bool)"), expected);
    let expected = IResult::Done(&[] as &[u8], Type::Ptr(Box::new(Type::Unit)));
    assert_eq!(type_expr(b"Ptr ()"), expected);
    let expected = IResult::Done(
        &[] as &[u8],
        Type::Tuple(
            vec![Type::Int, Type::Ptr(Box::new(Type::Ptr(Box::new(Type::Unit))))],
        ),
    );
    assert_eq!(type_expr(b"(Int, Ptr (Ptr ()))"), expected);
    eprintln!("");
    eprintln!("{:?}", decl(b"eq : Int -> Int -> Bool").unwrap());
    eprintln!("{:?}", defn(b"eq (Nat x) y = iszero (x - y)").unwrap());
    eprintln!("{:?}", decl(b"map : (Int -> Int) -> Int -> Int").unwrap());
    eprintln!("{:?}", defn(b"map f = f").unwrap());
    eprintln!("");
    let expected = IResult::Done(
        &[] as &[u8],
        DataDecl {
            name: "MyUnit".to_owned(),
            params: Vec::new(),
            variants: vec![("MyUnit".to_owned(), Vec::new())],
        },
    );
    assert_eq!(data_decl(b"data MyUnit = MyUnit"),expected);
    let expected = IResult::Done(
        &[] as &[u8],
        DataDecl {
            name: "MyInt".to_owned(),
            params: Vec::new(),
            variants: vec![("MyInt".to_owned(), vec![Type::Int])],
        },
    );
    assert_eq!(data_decl(b"data MyInt = MyInt Int"), expected);
    let expected = IResult::Done(
        &[] as &[u8],
        DataDecl {
            name: "Wrapper".to_owned(),
            params: vec!["a".to_owned()],
            variants: vec![("Wrapper".to_owned(), vec![Type::Param("a".to_owned())])],
        },
    );
    assert_eq!(data_decl(b"data Wrapper a = Wrapper a"), expected);
    let expected = IResult::Done(
        &[] as &[u8],
        DataDecl {
            name: "Union".to_owned(),
            params: Vec::new(),
            variants: vec![("Left".to_owned(), Vec::new()), ("Right".to_owned(), Vec::new())],
        },
    );
    assert_eq!(data_decl(b"data Union = Left | Right"), expected);
}
// pub fn parse(src: &str) -> IResult<&[u8], Node> {
// funcall(src.as_bytes())
// parse_expr(src.as_bytes(), 0)
// let_expr(src.trim().as_bytes())
// }
//
