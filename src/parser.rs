use std::str::{self, FromStr};
use nom::{IResult, IError, digit, alphanumeric};

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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Equal,
}

#[derive(Debug)]
pub struct Defn {
    name: String,
    args: Vec<Pattern>,
    val: Node,
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
        parse_bin_expr_full(i2,
                            prec,
                            Node::FunCall(Box::new(Node::FunCall(Box::new(Node::Op(tok)),
                                                                 Box::new(lhs))),
                                          Box::new(right)))
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
    b"Box" => map!(type_atom, |t| Type::Box(Box::new(t)))
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

named!(ident<String>, do_parse!(
	not!(reserved) >>
    not!(int) >>
	i: map!(map_res!(alphanumeric, str::from_utf8), String::from) >>
	(i)
));

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
    ws_nl!(tuple!(ident, many0!(pattern_atom))),
    |(c, v)| Pattern::Constructor(c, v)
));

named!(pattern_atom<Pattern>, alt_complete!(
    map!(tag!("_"), |_| Pattern::Wild) | map!(literal, Pattern::Lit) |
    map!(ident, Pattern::Ident) | tuple_pattern));
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

named!(type_atom<Type>, alt_complete!(builtin_type | tuple_type));
named!(type_expr<Type>, alt_complete!(func_type | type_atom));

named!(decl<(String, Type)>, ws_nl!(do_parse!(
	i: ident >>
	tag!(":") >>
	t: type_expr >>
	(i, t)
)));

named!(defn<Defn>, ws_nl!(do_parse!(
	i: ident >>
	a: ws_nl!(many0!(pattern_atom)) >>
	tag!("=") >>
	e: expr >>
	(Defn{name: i, args: a, val: e})
)));



pub fn test_lambda() {
    assert_eq!(expr(b"(\\x : Int -> x)"), IResult::Done([].as_ref(),
        Node::Lambda(Pattern::Ident("x".into()),
            Some(Type::Int), Box::new(Node::Ident("x".into())))));
}

pub fn test_exprs() {
    eprintln!("{:?}", expr(b"True").unwrap());
    eprintln!("{:?}", expr(b"False").unwrap());
    eprintln!("{:?}", expr(b"iszero e").unwrap());
    eprintln!("{:?}", expr(b"succ e").unwrap());
    eprintln!("{:?}", expr(b"pred e").unwrap());
    eprintln!("{:?}", expr(b"if e then e else e").unwrap());
    eprintln!("{:?}", expr(b"0").unwrap());
    eprintln!("");
    eprintln!("{:?}", type_expr(b"Int").unwrap());
    eprintln!("{:?}", type_expr(b"Bool").unwrap());
    eprintln!("{:?}", type_expr(b"(Int, Int)").unwrap());
    eprintln!("{:?}", type_expr(b"Int -> Int -> Bool").unwrap());
    eprintln!("");
    eprintln!("{:?}", decl(b"eq : Int -> Int -> Bool").unwrap());
    eprintln!("{:?}", defn(b"eq (Nat x) y = iszero (x - y)").unwrap());
    eprintln!("{:?}", decl(b"map : (Int -> Int) -> Int -> Int").unwrap());
    eprintln!("{:?}", defn(b"map f = f").unwrap());
    eprintln!("");
}
// pub fn parse(src: &str) -> IResult<&[u8], Node> {
// funcall(src.as_bytes())
// parse_expr(src.as_bytes(), 0)
// let_expr(src.trim().as_bytes())
// }
//
