use std::{collections::HashMap};
pub(crate) type Result<T> = std::result::Result<T, String>;
use inkwell::{self, builder::Builder, context::Context, module::Module, types::{BasicType, BasicTypeEnum}, values::{BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue}};
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

    fn build_add(&self, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>) -> Result<Option<BasicValueEnum>> {
        match &lhs.get_type() {
            BasicTypeEnum::ArrayType(_) => todo!(),
            BasicTypeEnum::FloatType(_) => Ok(Some(self.builder.build_float_add(lhs.into_float_value(), rhs.into_float_value(), "tmpfloatadd").as_basic_value_enum())),
            BasicTypeEnum::IntType(_) => Ok(Some(self.builder.build_int_add(lhs.into_int_value(), rhs.into_int_value(), "tmpintadd").as_basic_value_enum())),
            BasicTypeEnum::PointerType(_) => todo!(),
            BasicTypeEnum::StructType(_) => todo!(),
            BasicTypeEnum::VectorType(_) => todo!(),
        }
    }

    fn build_mul(&self, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>) -> Result<Option<BasicValueEnum>> {
        match &lhs.get_type() {
            BasicTypeEnum::ArrayType(_) => todo!(),
            BasicTypeEnum::FloatType(_) => Ok(Some(self.builder.build_float_mul(lhs.into_float_value(), rhs.into_float_value(), "tmpfloatmul").as_basic_value_enum())),
            BasicTypeEnum::IntType(_) => Ok(Some(self.builder.build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "tmpintmul").as_basic_value_enum())),
            BasicTypeEnum::PointerType(_) => todo!(),
            BasicTypeEnum::StructType(_) => todo!(),
            BasicTypeEnum::VectorType(_) => todo!(),
        }
    }

    fn build_sub(&self, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>) -> Result<Option<BasicValueEnum>> {
        match &lhs.get_type() {
            BasicTypeEnum::ArrayType(_) => todo!(),
            BasicTypeEnum::FloatType(_) => Ok(Some(self.builder.build_float_sub(lhs.into_float_value(), rhs.into_float_value(), "tmpfloatsub").as_basic_value_enum())),
            BasicTypeEnum::IntType(_) => Ok(Some(self.builder.build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "tmpintsub").as_basic_value_enum())),
            BasicTypeEnum::PointerType(_) => todo!(),
            BasicTypeEnum::StructType(_) => todo!(),
            BasicTypeEnum::VectorType(_) => todo!(),
        }
    }

    fn build_div(&self, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>) -> Result<Option<BasicValueEnum>> {
        match &lhs.get_type() {
            BasicTypeEnum::ArrayType(_) => todo!(),
            BasicTypeEnum::FloatType(_) => Ok(Some(self.builder.build_float_div(lhs.into_float_value(), rhs.into_float_value(), "tmpfloatdiv").as_basic_value_enum())),
            BasicTypeEnum::IntType(_) => Ok(Some(self.builder.build_int_signed_div(lhs.into_int_value(), rhs.into_int_value(), "tmpintdiv").as_basic_value_enum())),
            BasicTypeEnum::PointerType(_) => todo!(),
            BasicTypeEnum::StructType(_) => todo!(),
            BasicTypeEnum::VectorType(_) => todo!(),
        }
    }

    fn build_eq_comp(&self, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>) -> Result<Option<BasicValueEnum>> {
        match &lhs.get_type() {
            BasicTypeEnum::ArrayType(_) => todo!(),
            BasicTypeEnum::FloatType(_) => Ok(Some(self.builder.build_float_compare(inkwell::FloatPredicate::OEQ, lhs.into_float_value(), rhs.into_float_value(), "tmpfloatcmp").as_basic_value_enum())),
            BasicTypeEnum::IntType(_) => Ok(Some(self.builder.build_int_compare(inkwell::IntPredicate::EQ, lhs.into_int_value(), rhs.into_int_value(), "tmpintdiv").as_basic_value_enum())),
            BasicTypeEnum::PointerType(_) => todo!(),
            BasicTypeEnum::StructType(_) => todo!(),
            BasicTypeEnum::VectorType(_) => todo!(),
        }
    }


    fn build_assign(&self, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>) -> Result<Option<BasicValueEnum>> {
        self.builder.build_store(lhs.into_pointer_value(), rhs);
        Ok(None)
    }



    fn compile(&self, e: Expr) -> Result<Option<BasicValueEnum>> {
        match e {
            Expr::Binary(b) => {
                let lhs = gimme_opt!(gimme!(self.compile(b.left)), String::from("void"));
                let rhs = gimme_opt!(gimme!(self.compile(b.right)), String::from("void"));
                match b.op {
                    crate::parser::BinaryOps::Assign => self.build_assign(lhs, rhs),
                    crate::parser::BinaryOps::Add => self.build_add(lhs, rhs),
                    crate::parser::BinaryOps::Sub => self.build_sub(lhs, rhs),
                    crate::parser::BinaryOps::Mul => self.build_mul(lhs, rhs),
                    crate::parser::BinaryOps::Div => self.build_div(lhs, rhs),
                    crate::parser::BinaryOps::Equal => self.build_eq_comp(lhs, rhs),
                    crate::parser::BinaryOps::NEqual => todo!(),
                    crate::parser::BinaryOps::Less => todo!(),
                    crate::parser::BinaryOps::More => todo!(),
                    crate::parser::BinaryOps::LessEqual => todo!(),
                    crate::parser::BinaryOps::MoreEqual => todo!(),
                }
            },
            Expr::Variable(v) => {
                let val = self.variables.get(&v);
                match val {
                    Some(x) => {Ok(Some(x.as_basic_value_enum()))},
                    None => Err(format!("Cannot find variable {}", v)),
                }
            },
            Expr::If(i) => { // TODO: optimise so bools actually exist
                let cond = gimme_opt!(gimme!(self.compile(i.cond)), format!("Cannot use {:?} as boolean", i.cond));
                let comp: IntValue = match cond {
                    BasicValueEnum::ArrayValue(_) => todo!(),
                    BasicValueEnum::IntValue(i) => { self.builder.build_int_compare(inkwell::IntPredicate::NE, i, self.context.i8_type().const_zero(), "tmpintcmp")},
                    BasicValueEnum::FloatValue(_) => todo!(),
                    BasicValueEnum::PointerValue(_) => todo!(),
                    BasicValueEnum::StructValue(_) => todo!(),
                    BasicValueEnum::VectorValue(_) => todo!(),
                };
                let parent = gimme_opt!(self.fn_val, String::from("this shouldnt happen"));
                let then_bb = self.context.append_basic_block(parent, "then_branch");
                let else_bb = self.context.append_basic_block(parent, "else_branch");
                let cont_bb = self.context.append_basic_block(parent, "cont_branch");
                let branch = self.builder.build_conditional_branch(comp, then_bb, else_bb);

                self.builder.position_at_end(then_bb);
                let then_val = gimme!(self.compile(i.then));
                self.builder.build_unconditional_branch(cont_bb);

                self.builder.position_at_end(else_bb);
                let else_val: Option<BasicValueEnum> = if let Some(x) = i.els {
                    gimme!(self.compile(x))
                } else { None };
                self.builder.build_unconditional_branch(cont_bb);

                if let Some(t) = then_val {
                    if let Some(e) = else_val {
                        if t.get_type() == e.get_type() {
                            let phi = self.builder.build_phi(t.get_type(), "tmpifphi");
                            phi.add_incoming(&[
                                (&t, then_bb),
                                (&e, else_bb),
                            ]);
                            return Ok(Some(phi.as_basic_value()));
                        } else {
                            return Err(format!("Then-val not the same as else-val"));
                        }
                    }
                }

                Ok(None)
            },
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
            Expr::FnDef(f) => todo!(),
            Expr::StructDef(_) => todo!(),
        }
    }

    fn compile_fn(&self, f: Function) -> Result<FunctionValue> {
        let arg_types = vec![];
        let fn_type = self.resolve_type(f.proto.ty).fn;
    }
}