use std::{collections::HashMap};
pub(crate) type Result<T> = std::result::Result<T, String>;
use inkwell::{self, builder::Builder, context::Context, module::Module, types::{BasicType, BasicTypeEnum}, values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue}};
use crate::parser::{Expr, Function, Type};

macro_rules! gimme {
    ($x:expr) => {
        match $x {
            Ok(e) => e,
            Err(e) => return Err(e),
        };
    };
}

macro_rules! gimme_opt {
    ($x:expr, $y:expr) => {
        match $x {
            Some(x) => x,
            None => return Err($y),
        }
    };
}
pub(crate) struct Compiler<'ctx> {
    builder: Builder<'ctx>,
    context: &'ctx Context,
    module: Module<'ctx>,
    variables: HashMap<String, PointerValue<'ctx>>,
    fun: Option<Function>,
    fn_val: Option<FunctionValue<'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    fn new(name: String, c: &'ctx Context) -> Compiler<'ctx> {
        let builder = c.create_builder();
        let module = c.create_module(&name);
        let c = Compiler {builder, context: c, module, variables: HashMap::new(), fun: None, fn_val: None};
        return c;
    }

    fn resolve_type(&self, ty: Type) -> BasicTypeEnum {
        match ty {
            Type::I64 => self.context.i64_type().as_basic_type_enum(),
            Type::Struct(_) => todo!(),
        }
    }

    fn entry_block_alloca(&self, name: String, ty: BasicTypeEnum<'ctx>) -> Result<PointerValue<'ctx>> {
        let b = self.context.create_builder();
        let entry_block = self.fn_val.unwrap().get_first_basic_block().unwrap();
        match entry_block.get_first_instruction() {
            Some(i) => b.position_before(&i),
            None => b.position_at_end(entry_block),
        }
        Ok(b.build_alloca(ty, &name))
    }

    fn compile(&self, e: Expr) -> Result<Option<BasicValueEnum>> {
        match e {
            Expr::Binary(_) => todo!(),
            Expr::Variable(v) => {
                let val = self.variables.get(&v);
                match val {
                    Some(x) => {Ok(Some(x.as_basic_value_enum()))},
                    None => Err(format!("Cannot find variable {}", v)),
                }
            },
            Expr::If(_) => todo!(),
            Expr::Number(n) => {Ok(Some(self.context.i64_type().const_int(n as u64, false).as_basic_value_enum()))},
            Expr::Block(b) => {
                for i in b.statements {
                    let c = self.compile(i);
                    match c {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    }
                }
                Ok(None)
            },
            Expr::Def(d) => {
                let ty = self.resolve_type(d.typ);
                let m = self.entry_block_alloca(d.name.clone(), ty);
                let m = gimme!(m);
                self.variables.insert(d.name, m);
                Ok(None)
            },
            Expr::Call(c) => {
                let fun = self.module.get_function(&c.name);
                let fun = gimme_opt!(fun, format!("Unknown function {}", c.name));
                let args = vec![];
                for i in c.args {
                    args.push(gimme_opt!(gimme!(self.compile(i)), String::from("Cannot use as a non-void")));
                }
                let c = self.builder.build_call(fun, args.as_slice(), "tmpcall").try_as_basic_value().left().unwrap();
                Ok(Some(c))

            },
            Expr::FnDef(f) => {

            },
            Expr::StructDef(_) => todo!(),
        }
    }
}