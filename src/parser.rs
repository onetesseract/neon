use std::collections::HashMap;

use crate::lexer::{self, LexValue, Lexer};
use crate::{Result, Error};

macro_rules! gimme {
    ($x:expr) => {
        match $x {
            Ok(e) => e,
            Err(e) => return Err(e),
        };
    };
}

#[derive(Debug, Clone)]
pub(crate) enum BinaryOps {
    Assign,

    Add,
    Sub,
    Mul,
    Div,
    Equal,
    NEqual,
    Less,
    More,
    LessEqual,
    MoreEqual,
}

#[derive(Debug, Clone)]
pub(crate) struct Binary {
    left: Expr,
    op: BinaryOps,
    right: Expr,
}

#[derive(Debug, Clone)]
pub(crate) struct If {
    cond: Expr,
    then: Expr,
    els: Option<Expr>,
}

#[derive(Debug, Clone)]
pub(crate) struct Block {
    statements: Vec<Expr>
}

#[derive(Debug, Clone)]
pub(crate) struct Def {
    name: String,
    typ: Type,
}

#[derive(Debug, Clone)]
pub(crate) struct Prototype {
    name: String,
    args: Vec<Def>,
    ty: Type,
}

#[derive(Debug, Clone)]
pub(crate) struct Call {
    name: String,
    args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub(crate) struct Function {
    proto: Prototype,
    body: Expr,
}

#[derive(Debug, Clone)]
pub(crate) enum Expr {
    Binary(Box<Binary>),
    Variable(String),
    If(Box<If>),
    Number(isize),
    Block(Block),
    Def(Def),
    Call(Call),
    FnDef(Box<Function>),
    StructDef(StructDef),
}

#[derive(Debug, Clone)]
pub(crate) struct StructDef {
    name: String,
    vals: Vec<Def>,
}

#[derive(Debug, Clone)]
pub(crate) enum Type {
    I64,
    Struct(String),
}

#[derive(Debug, Clone)]
pub(crate) struct Parser {
    lexer: lexer::Lexer,
    types: HashMap<String, Type>,
}

impl Parser {
    pub(crate) fn new(l: Lexer, t: HashMap<String, Type>) -> Parser {
        return Parser {lexer: l, types: t};
    }
    fn parse_punc(&mut self, v: LexValue) -> Result<Expr> {
        match v.clone().get().as_bytes()[0] as char {
            '(' => {
                let e = self.parse_maybe();
                if self.lexer.cont.as_bytes()[self.lexer.index] as char != ')' {
                    Err(Error::new(format!("Expected a closing bracket to match that at {}", v.start), self.lexer.index))
                } else {
                    self.lexer.index+=1;
                    e
                }
            }
            '{' => {
                let e = self.parse_block();
                e
            }
            _ => Err(Error::new(String::from("Unexpected punctation literal"), v.start)),
        }
    }

    fn parse_if(&mut self) -> Result<Expr> {
        let cond = self.parse_maybe();
        let cond = match cond {
            Ok(c) => c,
            Err(e) => return Err(e),
        };
        let then = self.parse_maybe();
        let then = match then {
            Ok(t) => t,
            Err(e) => return Err(e),
        };
        let mut els = None;
        match self.lexer.clone().lex() {
            Ok(l) => {
                if l.get_val().get() == String::from("else") {
                    self.lexer.lex().unwrap();
                    let e = self.parse_maybe();
                    let e = match e {
                        Ok(e) => e,
                        Err(e) => return Err(e),
                    };
                    els = Some(e);
                }
            },
            Err(_) => (),
        }
        let i = If {cond, then, els};
        return Ok(Expr::If(Box::new(i)));
    }

    fn parse_block(&mut self) -> Result<Expr> {
        let mut statements = vec![];
        let mut is_defs = true;
        loop {
            if let Ok(x) =  self.lexer.clone().lex() {
                if x.get_val().get() == String::from("}") {
                    self.lexer.lex().unwrap();
                    break;
                }
                let s = self.parse_maybe();
                let s = gimme!(s);
                if !matches!(s, Expr::Def(_)) { is_defs = false; }
                statements.push(s);
            } else {
                let e = self.lexer.lex().unwrap_err();
                return Err(e);
            }
        }
        if is_defs {
            if let Ok(Expr::Variable(x)) =  self.clone().parse_maybe() {
                self.clone().parse_maybe().unwrap();
                let mut defs = vec![];
                for i in statements {
                    if let Expr::Def(d) = i {
                        defs.push(d);
                    } else { panic!(); }
                }
                let d = StructDef {
                    name: x.clone(), vals: defs,                    
                };
                self.types.insert(x.clone(), Type::Struct(x));
                return Ok(Expr::StructDef(d));

            }
        }
        return Ok(Expr::Block(Block {statements}));
    }
    
    fn parse_fndef(&mut self, args: Vec<Expr>, v: LexValue) -> Result<Expr> {
        if let Ok(lexer::LexToken::Id(x)) = self.lexer.clone().lex() {
            self.lexer.lex().unwrap();
            let ty = &self.types.get(&x.get()).unwrap().clone();
            let body = self.parse_maybe();
            let body = gimme!(body);
            let mut a = vec![];
            for i in args {
                match i {
                    Expr::Def(d) => a.push(d),
                    x => return Err(Error::new(format!("Cannot use {:?} as a variable def within a function prototype", x), v.start))
                }
            }
            let proto = Prototype { name: v.get(), args: a, ty: ty.clone()};
            let f = Function { proto, body };
            return Ok(Expr::FnDef(Box::new(f)));
        } else {panic!()}
    }

    fn parse_call(&mut self, v: LexValue) -> Result<Expr> {
        self.lexer.lex().unwrap();
        let mut args = vec![];
        loop {
            if let Ok(lexer::LexToken::Punc(x)) = self.lexer.clone().lex() { if x.get() == String::from(")") { self.lexer.lex().unwrap(); break; } }
            match self.parse_maybe() {
                Ok(p) => args.push(p),
                Err(e) => return Err(e),
            }
        }
        if let Ok(lexer::LexToken::Id(x)) = self.lexer.clone().lex() { if self.types.contains_key(&x.get()) { return self.parse_fndef(args, v)} }
        let p = Call {name: v.get(), args};
        Ok(Expr::Call(p))


    }

    fn parse_id(&mut self, v: LexValue) -> Result<Expr> {
        match &v.clone().get() as &str {
            "if" => self.parse_if(),
            x => {
                let peeked = self.lexer.clone().lex();
                if let Ok(lexer::LexToken::Punc(x)) = &peeked { if x.get() == String::from("(") { return self.parse_call(v) } }
                if let Ok(lexer::LexToken::Id(_x)) = peeked {if self.types.contains_key(&_x.get()) { 
                    self.lexer.lex().unwrap();
                    let d = Def {name: x.to_string(), typ: self.types.get(&_x.get()).unwrap().clone()};
                    return Ok(Expr::Def(d));
                } }
                Ok(Expr::Variable(x.to_string()))
            }
        }
    }
    fn maybe_binary(&mut self, e: Expr) -> Result<Expr> {
        if let Ok(lexer::LexToken::Op(o)) = self.lexer.clone().lex() {
            self.lexer.lex().unwrap();
            let op = match &o.get() as &str {
                "+" => BinaryOps::Add,
                "-" => BinaryOps::Sub,
                "*" => BinaryOps::Mul,
                "/" => BinaryOps::Div,
                "==" => BinaryOps::Equal,
                "!=" => BinaryOps::NEqual,
                "<" => BinaryOps::Less,
                ">" => BinaryOps::More,
                "<="|"=<" => BinaryOps::LessEqual,
                ">="|"=>" => BinaryOps::MoreEqual,
                "=" => BinaryOps::Assign,
                _ => return Err(Error::new(format!("BUG: Unknown binary operator {}", o.get()), o.start)),
            };
            let rhs = self.parse_maybe();
            let rhs = match rhs {
                Ok(rhs) => rhs,
                Err(e) => return Err(e),
            };
            let b = Binary {left: e, op: op, right: rhs};
            return Ok(Expr::Binary(Box::new(b)));
        }
        return Ok(e);
    }
    fn parse_number(&mut self, n: LexValue) -> Result<Expr> {
        let s = n.get();
        let num = s.parse::<isize>();
        let num = match num {
            Ok(num) => num,
            Err(_) => return Err(Error {msg: format!("I can't parse {} as a number.", s), index: n.start})
        };
        return Ok(Expr::Number(num));
    }

    pub(crate) fn parse_maybe(&mut self) -> Result<Expr> {
        let p = self.parse();
        let p = match p {
            Ok(p) => p,
            Err(e) => return Err(e),
        };
        let mb = self.maybe_binary(p);
        let mb = match mb {
            Ok(mb) => mb,
            Err(e) => return Err(e),
        };
        return Ok(mb);
    }
    fn parse(&mut self) -> Result<Expr> {
        let l = self.lexer.lex();
        let l = match l {
            Ok(l) => l,
            Err(e) => return Err(e),
        };
        let e = match l {
            lexer::LexToken::Punc(p) => self.parse_punc(p),
            lexer::LexToken::Id(i) => self.parse_id(i),
            lexer::LexToken::Op(_) => todo!(),
            lexer::LexToken::Number(n) => self.parse_number(n),
        };
        return e;

    }
}
