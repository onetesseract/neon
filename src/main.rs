mod lexer;
mod parser;

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub(crate) struct Error {
    msg: String,
    index: usize,
}

impl Error {
    pub(crate) fn new(msg: String, index: usize) -> Error {
        Error {msg, index}
    }
}

pub(crate) type Result<T> = std::result::Result<T, Error>;

fn main() {
    //println!("Hello, world!");
    let mut l = lexer::Lexer::new(String::from("test.neon"));
    let mut t = HashMap::new();
    t.insert(String::from("i64"), parser::Type::I64);
    let mut p = parser::Parser::new(&mut l, &mut t);
    let _p = p.parse_maybe();
    println!("{:?}", _p);

}
