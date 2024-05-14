use sexp::Atom::*;
use sexp::Sexp;

#[derive(Debug)]
pub struct Prog {
    pub defs: Vec<Defn>,
    pub expr: Box<Expr>,
}

#[derive(Debug)]
pub enum Defn {
    Fun(String, Vec<String>, Box<Expr>),
}

#[derive(Debug)]
pub enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i32),
    Label(String),
}

#[derive(Debug)]
pub enum Reg {
    RAX,
    RSP,
    RDI,
    RCX,
    RBP,
    R11,
}

#[derive(Debug)]
pub enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    IJe(Val),
    IJne(Val),
    IJg(Val),
    IJge(Val),
    IJl(Val),
    IJle(Val),
    IJo(Val),
    ICmp(Val, Val),
    IJmp(Val),
    IAnd(Val, Val),
    DefLabel(String),
    IPush(Val),
    ICall(Val),
    IXor(Val, Val),
    IShr(Val, Val),
    IRet,
    ISar(Val, Val),
    ICmove(Val, Val),
    IPop(Val),
    Comment(String),
}

#[derive(Debug)]
pub enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
}

#[derive(Debug)]
pub enum Op2 {
    Plus,
    Minus,
    Times,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Debug)]
pub enum Expr {
    Number(i64),
    Boolean(bool),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
    Input,
    Print(Box<Expr>),
    Call(String, Vec<Expr>),
}

// Parsers
fn parse_bind(bindings: &Sexp, exprs: &mut Vec<(String, Expr)>, in_def: bool) {
    let reserved_names = vec!["true", "false", "input", "add1", "sub1",
     "isnum", "isbool", "let", "if", "set!", "block", "loop", "break", "fun"];
    match bindings {
        Sexp::List(vec) => {
            for binding in vec {
                if let Sexp::List(binding_vec) = binding {
                    if let [Sexp::Atom(S(x)), e] = &binding_vec[..] {
                        if reserved_names.contains(&x.as_str()) {
                            panic!("Invalid keyword: {}", x);
                        }
                        exprs.push((x.to_owned(), parse_expr(e, in_def)));
                    } else {
                        panic!("Invalid binding format");
                    }
                } else {
                    panic!("Invalid binding format");
                }
            }
        }
        _ => panic!("Invalid binding format"),
    }
}

fn parse_ident(s: &Sexp) -> String {
    let reserved_names = vec!["true", "false", "input", "add1", "sub1",
    "isnum", "isbool", "let", "if", "set!", "block", "loop", "break", "fun"];
    match s {
        Sexp::Atom(S(x)) => {
            if reserved_names.contains(&x.as_str()) {
                panic!("Invalid function header name: {}", x);
            }
            x.to_string()
        } 
        _ => panic!("Invalid: parse error"),
    }
}

pub fn parse_defn(s: &Sexp) -> Defn {
    let Sexp::List(es) = s else {
        panic!("Invalid syntax error: expected a list")
    };
    match &es[..] {
        [Sexp::Atom(S(op)), Sexp::List(es), body] if op == "fun" => {
            let [name, params @ ..] = &es[..] else {
                panic!("Invalid: missing function name");
            };
            let body = Box::new(parse_expr(body, true));
            let name: String = parse_ident(name);
            Defn::Fun(name, params.iter().map(|param| parse_ident(param)).collect(), body)
        }
        _ => panic!("Invalid syntax error: expected a list of 4 elements"),
    }
}

fn parse_prog(e: &Sexp) -> Prog {
    let Sexp::List(es) = e else {
        panic!("Invalid syntax error: expected a list")
    };
    // println!("{:?} with length = {}", es, es.len());

    if let [defs @ .., expr] = &es[..] {
        let defs = defs.iter().map(|e| parse_defn(e)).collect();
        let expr = Box::new(parse_expr(expr, false));
        Prog { defs, expr }
    } else {
        panic!("Invalid syntax error: program must contain a main expression")
    }
}

pub fn parse_expr(s: &Sexp, in_def: bool) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            if *n > (1 << 62) - 1 || *n < -(1 << 62) {
                panic!("Invalid int: overflow");
            }
            Expr::Number(i64::try_from(*n).expect("Invalid number"))
        }
        Sexp::Atom(S(s)) if s == "true" => Expr::Boolean(true),
        Sexp::Atom(S(s)) if s == "false" => Expr::Boolean(false),
        Sexp::Atom(S(s)) if s == "input" => {
            if in_def {
                panic!("Invalid: input cannot be used in function body")
            }
            Expr::Input
        },

        Sexp::Atom(S(s)) => Expr::Id(s.clone()),

        Sexp::List(vec) => {
            match &vec[..] {
                // Unary Ops
                [Sexp::Atom(S(op)), e] if op == "add1" => {
                    Expr::UnOp(Op1::Add1, Box::new(parse_expr(e, in_def)))
                }
                [Sexp::Atom(S(op)), e1] if op == "sub1" => {
                    Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e1, in_def)))
                }
                [Sexp::Atom(S(op)), e1] if op == "isnum" => {
                    Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e1, in_def)))
                }
                [Sexp::Atom(S(op)), e1] if op == "isbool" => {
                    Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e1, in_def)))
                }
                // Binary Ops
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(
                    Op2::Plus,
                    Box::new(parse_expr(e1, in_def)),
                    Box::new(parse_expr(e2, in_def)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(
                    Op2::Minus,
                    Box::new(parse_expr(e1, in_def)),
                    Box::new(parse_expr(e2, in_def)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(
                    Op2::Times,
                    Box::new(parse_expr(e1, in_def)),
                    Box::new(parse_expr(e2, in_def)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(
                    Op2::Greater,
                    Box::new(parse_expr(e1, in_def)),
                    Box::new(parse_expr(e2, in_def)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(
                    Op2::Less,
                    Box::new(parse_expr(e1, in_def)),
                    Box::new(parse_expr(e2, in_def)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(
                    Op2::GreaterEqual,
                    Box::new(parse_expr(e1, in_def)),
                    Box::new(parse_expr(e2, in_def)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(
                    Op2::LessEqual,
                    Box::new(parse_expr(e1, in_def)),
                    Box::new(parse_expr(e2, in_def)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(
                    Op2::Equal,
                    Box::new(parse_expr(e1, in_def)),
                    Box::new(parse_expr(e2, in_def)),
                ),
                // Keywords
                [Sexp::Atom(S(op)), bindings, e2] if op == "let" => {
                    let mut exprs = vec![];
                    parse_bind(bindings, &mut exprs, in_def);
                    let e2 = parse_expr(e2, in_def);
                    Expr::Let(exprs, Box::new(e2))
                }
                [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => Expr::If(
                    Box::new(parse_expr(e1, in_def)),
                    Box::new(parse_expr(e2, in_def)),
                    Box::new(parse_expr(e3, in_def)),
                ),
                [Sexp::Atom(S(op)), Sexp::Atom(S(x)), e] if op == "set!" => {
                    Expr::Set(x.to_owned(), Box::new(parse_expr(e, in_def)))
                }
                [Sexp::Atom(S(op)), exprs @..] if op == "block" => {
                    Expr::Block(exprs.into_iter().map(|expr| parse_expr(expr, in_def)).collect())
                }
                [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e, in_def))),
                [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e, in_def))),
                [Sexp::Atom(S(op)), e] if op == "print" => Expr::Print(Box::new(parse_expr(e, in_def))),
                [Sexp::Atom(S(f)), params @..] => {
                    let parsed_args = params.iter().map(|param| parse_expr(param, in_def)).collect();
                    Expr::Call(f.to_owned(), parsed_args)
                },
                _ => panic!("Invalid parse {}", s),
            }
        }
        _ => panic!("Invalid parse {}", s),
    }
}

pub fn parse(s: &str) -> Prog {
    let s = format!("({})", s);
    let s = sexp::parse(&s).unwrap_or_else(|_| panic!("invalid s-expr"));
    parse_prog(&s)
}