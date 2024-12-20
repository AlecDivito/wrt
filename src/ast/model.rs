// use std::convert::TryFrom;

// use crate::{
//     ast::types::NumberType,
//     execution::Number,
//     lex::{
//         ast::{read_number, read_u32, Expect, TryGet},
//         Keyword,
//     },
//     structure::types::Index,
// };

// use super::{
//     error::Error,
//     tree::{Node, NodeVisitor},
//     types::ValueType,
// };

#[derive(Debug, Default, Clone)]
pub struct Str(String);

impl Str {
    pub fn new(number: String) -> Self {
        Str(number)
    }
}

impl<V: NodeVisitor> Accept<V> for Str {
    fn accept(&self, v: &mut V) -> Result<(), Error> {
        v.visit_string(self)
    }
}

#[derive(Debug, Default, Clone, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct Id(pub String);

impl Id {
    pub fn new(number: String) -> Self {
        Id(number)
    }
}

impl<V: NodeVisitor> Accept<V> for Id {
    fn accept(&self, v: &mut V) -> Result<(), Error> {
        v.visit_id(self)
    }
}

#[derive(Debug, Clone)]
pub struct NumberLiteral {
    pub token: Token,
}

impl NumberLiteral {
    pub fn new(token: Token) -> Self {
        Self { token }
    }
}

impl<V: NodeVisitor> Accept<V> for NumberLiteral {
    fn accept(&self, v: &mut V) -> Result<(), Error> {
        v.visit_number_literal(self)
    }
}

#[derive(Debug)]
pub enum VariableOp {
    Get(Index),
}

impl<V: NodeVisitor> Accept<V> for VariableOp {
    fn accept(&self, v: &mut V) -> Result<(), Error> {
        v.visit_variable_op(self)
    }
}

#[derive(Debug)]
pub enum BinaryOp {
    Add(NumberType),
}

impl<V: NodeVisitor> Accept<V> for BinaryOp {
    fn accept(&self, v: &mut V) -> Result<(), Error> {
        v.visit_number_binary_op(self)
    }
}

#[derive(Debug, Default, Clone)]
pub struct ResultNode {
    tys: Vec<ValueType>,
}

impl<V: NodeVisitor> Accept<V> for ResultNode {
    fn accept(&self, v: &mut V) -> std::result::Result<(), Error> {
        v.visit_result(self)
    }
}

impl NodeVisitor for ResultNode {
    fn visit_value_type(&mut self, value: &ValueType) -> Result<(), Error> {
        self.tys.push(value.clone());
        Ok(())
    }
}

// impl TryFrom<&mut Node> for ResultNode {
//     type Error = Error;

//     fn try_from(node: &mut Node) -> Result<Self, Self::Error> {
//         let mut this = Self::default();
//         assert_eq!(node.root, Keyword::Result);

//         while let Some(attribute) = node.take_left() {
//             this.types.push(ValueType::try_from(attribute)?);
//         }

//         Ok(this)
//     }
// }

#[derive(Debug, Default, Clone)]
pub struct Parameter {
    id: Option<Id>,
    tys: Vec<ValueType>,
}

impl Parameter {
    pub fn from_str(&self, string: impl AsRef<str>) -> (Option<Id>, Value) {
        match self.tys.first().unwrap() {
            ValueType::Number(NumberType::I32) => (
                self.id.clone(),
                Value::Number(Number::I32(string.as_ref().parse::<i32>().unwrap())),
            ),
            ValueType::Number(NumberType::I64) => (
                self.id.clone(),
                Value::Number(Number::I64(string.as_ref().parse::<i64>().unwrap())),
            ),
            ValueType::Number(NumberType::F32) => (
                self.id.clone(),
                Value::Number(Number::F32(string.as_ref().parse::<f32>().unwrap())),
            ),
            ValueType::Number(NumberType::F64) => (
                self.id.clone(),
                Value::Number(Number::F64(string.as_ref().parse::<f64>().unwrap())),
            ),
            ValueType::Vector(vector_type) => todo!(),
            ValueType::Ref(reference_type) => todo!(),
        }
    }

    pub fn id(&self) -> Option<&Id> {
        self.id.as_ref()
    }
}

impl<V: NodeVisitor> Accept<V> for Parameter {
    fn accept(&self, v: &mut V) -> Result<(), Error> {
        v.visit_param(self)
    }
}

impl NodeVisitor for Parameter {
    fn visit_id(&mut self, id: &Id) -> Result<(), Error> {
        self.id = Some(id.clone());
        Ok(())
    }

    fn visit_value_type(&mut self, value: &ValueType) -> Result<(), Error> {
        self.tys.push(value.clone());
        Ok(())
    }
}

#[derive(Debug, Default, Clone)]
pub struct Local {
    id: Option<Id>,
    ty: Option<ValueType>,
}

impl<V: NodeVisitor> Accept<V> for Local {
    fn accept(&self, v: &mut V) -> Result<(), Error> {
        v.visit_local(self)
    }
}

impl NodeVisitor for Local {
    fn visit_id(&mut self, id: &Id) -> Result<(), Error> {
        self.id = Some(id.clone());
        Ok(())
    }

    fn visit_value_type(&mut self, value: &ValueType) -> Result<(), Error> {
        self.ty = Some(value.clone());
        Ok(())
    }
}

// impl TryFrom<&mut Node> for Parameter {
//     type Error = Error;

//     fn try_from(node: &mut Node) -> Result<Self, Self::Error> {
//         let mut this = Self::default();
//         assert_eq!(node.root, Keyword::Param);
//         this.id = node.take_left().try_id();

//         while let Some(attribute) = node.take_left() {
//             this.types.push(ValueType::try_from(attribute)?);
//         }

//         Ok(this)
//     }
// }

// #[derive(Debug, Default)]
// pub struct LocalNode {
//     id: Option<String>,
//     types: Vec<ValueType>,
// }

// impl TryFrom<&mut Node> for LocalNode {
//     type Error = Error;

//     fn try_from(node: &mut Node) -> Result<Self, Self::Error> {
//         let mut this = Self::default();
//         assert_eq!(node.root, Keyword::Local);
//         this.id = node.take_left().try_id();

//         while let Some(attribute) = node.take_left() {
//             this.types.push(ValueType::try_from(attribute)?);
//         }

//         Ok(this)
//     }
// }

// #[derive(Debug, Default)]
// pub struct FuncTypeNode {
//     params: Vec<ParamNode>,
//     result: Vec<ResultNode>,
// }

// impl TryFrom<&mut Node> for FuncTypeNode {
//     type Error = Error;

//     fn try_from(node: &mut Node) -> Result<Self, Self::Error> {
//         let mut this = Self::default();
//         assert_eq!(node.root, Keyword::Func);

//         while let Some(child_node) = node.take_child_if(Keyword::Param) {
//             this.params.push(ParamNode::try_from(child_node)?);
//         }

//         while let Some(child_node) = node.take_child_if(Keyword::Result) {
//             this.result.push(ResultNode::try_from(child_node)?);
//         }

//         Ok(this)
//     }
// }

use std::{collections::HashMap, str::FromStr};

use crate::{
    execution::{instance::Value, Number},
    lex::Token,
    structure::types::Index,
    validation::{Context, Input, ValidateInstruction, ValidateResult},
};

use super::{
    error::Error,
    tree::{Accept, Expression, NodeVisitor},
    types::{NumberType, ValueType},
};

#[derive(Debug, Default, Clone)]
pub struct FunctionType {
    id: Option<String>,
    // functype: Vec<FuncTypeNode>,
}

impl<V: NodeVisitor> Accept<V> for FunctionType {
    fn accept(&self, v: &mut V) -> Result<(), Error> {
        v.visit_type(self)
    }
}

impl NodeVisitor for FunctionType {
    fn visit_id(&mut self, id: &Id) -> Result<(), Error> {
        self.id = Some(id.0.clone());
        Ok(())
    }

    fn visit_func(&mut self, func: &Function) -> Result<(), Error> {
        todo!()
    }
}

// impl TryFrom<&mut Node> for FunctionType {
//     type Error = Error;

//     fn try_from(node: &mut Node) -> Result<Self, Self::Error> {
//         let mut this = Self::default();
//         assert_eq!(node.root, Keyword::Type);
//         this.id = node.take_left().try_id();

//         if let Some(node) = node.take_child_if(Keyword::Func) {
//             this.functype = Some(FuncTypeNode::try_from(node)?);
//         }

//         Ok(this)
//     }
// }

// pub enum CompositeType {
//     Func(FunctionType),
// }

// pub struct SubType {
//     is_final: bool,
//     type_idx: Vec<String>,
//     composition: CompositeType,
// }

// pub struct RecusiveType {
//     tys: Vec<SubType>,
// }

// #[derive(Debug, Default)]
// pub struct StartNode {}

// /*
// nn,mm ::= 32|64
// sx       ::= u|s
// instr    ::= inn.const unn | fnn.const f nn
// | inn .iunop | f nn .funop
// | inn .ibinop
// | inn .itestop
// | inn .irelop | f nn .frelop
// | inn .extend8_s | inn .extend16_s | i64.extend32_s
// | i32.wrap_i64 | i64.extend_i32_sx | inn .trunc_f mm _sx
// | inn .trunc_sat_f mm _sx
// | f32.demote_f64 | f64.promote_f32 | f nn .convert_imm _sx | inn .reinterpret_f nn | f nn .reinterpret_inn
// | ...

// iunop  ::= clz | ctz | popcnt
// ibinop ::= add | sub | mul | div_sx | rem_sx | and|or|xor|shl|shr_sx |rotl|rotr

// funop   ::= abs|neg|sqrt|ceil|floor|trunc|nearest
// fbinop  ::= add|sub|mul|div|min|max|copysign
// itestop ::= eqz
// irelop  ::= eq|ne|lt_sx |gt_sx |le_sx |ge_sx
// frelop  ::= eq|ne|lt|gt|le|ge

// unop   ::= iunop | funop | extend𝑁_s
// binop  ::= ibinop | fbinop
// testop ::= itestop
// relop  ::= irelop | frelop
// cvtop  ::= wrap | extend | trunc | trunc_sat | convert | demote | promote | reinterpret
// */
// // #[derive(Debug, Default)]
// // pub enum InstructionNode {
// //     Constant {
// //         op:
// //         op: NumberType
// //     }, // return a static constant.
// //     Unary(NumberType), // consume one operand and produce one result of the respective type.
// //     • Binary Operations: consume two operands and produce one result of the respective type.
// //     • Tests: consume one operand of the respective type and produce a Boolean integer result.
// //     • Comparisons: consume two operands of the respective type and produce a Boolean integer result.
// //     • Conversions: consume a value of one type and produce a result of another (the source type of the conversion is the one after the “_”).

// // }

// #[derive(Debug)]
// pub enum Expression {
//     I32Add,
//     LocalGet(Index),
//     Literal(String),
// }

// #[derive(Debug)]
// pub struct InstructionNode {
//     expressions: Vec<Expression>,
// }

// impl TryFrom<&mut Node> for InstructionNode {
//     type Error = Error;

//     fn try_from(node: &mut Node) -> Result<Self, Self::Error> {
//         assert!(node.root.is_instruction());

//         let mut expressions = vec![];

//         for node in node.children_mut() {
//             expressions.extend(InstructionNode::try_from(node)?.expressions)
//         }

//         expressions.push(if let Ok(literal) = Some(&node.token).expect_number() {
//             Expression::Literal(literal.source.clone())
//         } else {
//             match node.root {
//                 Keyword::LocalGet => Expression::LocalGet(Index::try_from(node)?),
//                 Keyword::Add(NumberType::I32) => Expression::I32Add,
//                 _ => todo!("Correctlly error out"),
//             }
//         });

//         Ok(InstructionNode { expressions })
//     }
// }

#[derive(Debug, Clone)]
pub enum ExportType {
    Func(Index),
}

#[derive(Debug, Default, Clone)]
pub struct Export {
    name: Option<String>,
    ty: Option<ExportType>,
}

impl Export {
    pub fn ty(&self) -> Option<&ExportType> {
        self.ty.as_ref()
    }

    pub fn name(&self) -> Option<&String> {
        self.name.as_ref()
    }
}

impl<V: NodeVisitor> Accept<V> for Export {
    fn accept(&self, v: &mut V) -> Result<(), Error> {
        v.visit_export(self)
    }
}

impl NodeVisitor for Export {
    fn visit_string(&mut self, item: &Str) -> Result<(), Error> {
        self.name = Some(item.0.clone());
        Ok(())
    }

    fn visit_func(&mut self, item: &Function) -> Result<(), Error> {
        if let Some(id) = item.id.as_ref() {
            self.ty = Some(ExportType::Func(Index::Id(id.clone())));
        }
        Ok(())
    }
}

pub struct Frame {
    params: Vec<Value>,
    locals: HashMap<Id, usize>,
    stack: Vec<Value>,
}

impl Frame {
    pub fn from_strings(func: &Function, parameters: &[&str]) -> Self {
        let parameters = func
            .params()
            .iter()
            .zip(parameters)
            .map(|(ty, param)| ty.from_str(param))
            .collect::<Vec<_>>();
        let mut params = vec![];
        let mut locals = HashMap::new();
        for (index, (id, input_param)) in parameters.iter().enumerate() {
            params.push(input_param.clone());
            if let Some(id) = id {
                locals.insert(id.clone(), index);
            }
        }

        Frame {
            params,
            locals,
            stack: vec![],
        }
    }

    pub fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    pub fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    pub fn local(&self, index: &Index) -> Value {
        match index {
            Index::Id(name) => {
                let index = self.locals.get(&Id(name.clone())).unwrap();
                let value = (*self).params.get(*index).unwrap().clone();
                value
            }
            Index::Index(id) => (*self).params.get(*id as usize).unwrap().clone(),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Function {
    id: Option<String>,

    ty: Option<FunctionType>,
    params: Vec<Parameter>,
    results: Vec<ResultNode>,
    locals: Vec<Local>,
    body: Vec<Expression>,
}

impl Function {
    pub fn id(&self) -> Option<&String> {
        self.id.as_ref()
    }

    pub fn call(&self, mut frame: Frame) -> Vec<Value> {
        // Execution loop
        for instruction in self.body.iter() {
            instruction.execute(&mut frame);
        }

        frame.stack
    }

    pub fn params(&self) -> &[Parameter] {
        &self.params
    }
}

impl<V: NodeVisitor> Accept<V> for Function {
    fn accept(&self, v: &mut V) -> Result<(), Error> {
        v.visit_func(self)
    }
}

impl NodeVisitor for Function {
    fn visit_id(&mut self, id: &Id) -> Result<(), Error> {
        self.id = Some(id.0.clone());
        Ok(())
    }

    fn visit_param(&mut self, param: &Parameter) -> Result<(), Error> {
        self.params.push(param.clone());
        Ok(())
    }

    fn visit_result(&mut self, result: &ResultNode) -> Result<(), Error> {
        self.results.push(result.clone());
        Ok(())
    }

    fn visit_type(&mut self, ty: &FunctionType) -> Result<(), Error> {
        self.ty = Some(ty.clone());
        Ok(())
    }

    fn visit_local(&mut self, var: &Local) -> Result<(), Error> {
        self.locals.push(var.clone());
        Ok(())
    }

    fn visit_expression(&mut self, expression: &Expression) -> Result<(), Error> {
        self.body.push(expression.clone());
        Ok(())
    }
}

// impl TryFrom<&mut Node> for Function {
//     type Error = Error;

//     fn try_from(node: &mut Node) -> Result<Self, Self::Error> {
//         let mut this = Self::default();
//         assert_eq!(node.root, Keyword::Func);
//         this.id = node.take_left().try_id();

//         this.ty = node.take_and_try_child_if(Keyword::Type)?;

//         while let Some(node) = node.take_child_if(Keyword::Param) {
//             this.params.push(ParamNode::try_from(node)?);
//         }

//         while let Some(node) = node.take_child_if(Keyword::Result) {
//             this.results.push(ResultNode::try_from(node)?);
//         }

//         while let Some(node) = node.take_child_if(Keyword::Local) {
//             this.locals.push(LocalNode::try_from(node)?);
//         }

//         while let Some(node) = node.take_child_if_func(|n| n.root.is_instruction()) {
//             this.instructions.push(InstructionNode::try_from(node)?);
//         }

//         Ok(this)
//     }
// }

#[derive(Debug, Default)]
pub struct Module {
    id: Option<String>,
    pub(crate) types: Vec<FunctionType>,
    pub(crate) functions: Vec<Function>,
    pub(crate) exports: Vec<Export>,
    // tables: Vec<TableNode>,
    // memories: Vec<MemoryNode>,
    // globals: Vec<GlobalNode>,
    // elements: Vec<ElementNode>,
    // datas: Vec<DataNode>,
    // imports: Vec<ImportNode>,
    pub(crate) export: Vec<Export>,
    // start: Option<StartNode>,
}

impl<V: NodeVisitor> Accept<V> for Module {
    fn accept(&self, v: &mut V) -> Result<(), Error> {
        v.visit_module(self)
    }
}

impl NodeVisitor for Module {
    fn visit_type(&mut self, func: &FunctionType) -> Result<(), Error> {
        self.types.push(func.clone());
        Ok(())
    }

    fn visit_func(&mut self, func: &Function) -> Result<(), Error> {
        self.functions.push(func.clone());
        Ok(())
    }

    fn visit_export(&mut self, item: &Export) -> Result<(), Error> {
        self.exports.push(item.clone());
        Ok(())
    }
}

impl ValidateInstruction for Module {
    fn validate(&self, ctx: &mut Context, inputs: &mut Input) -> ValidateResult<Vec<ValueType>> {
        todo!()
    }
}

// impl NodeVisitor for Module {
//     fn visit_id(&mut self, id: &str) -> Result<(), Error> {
//         self.id = Some(id.to_string());
//         Ok(())
//     }

//     fn visit_keyword(&mut self, node: &Node) -> Result<(), Error> {
//         for children in node.children() {
//             match children {
//                 super::tree::Item::Token(token) => {
//                     match
//                 },
//                 super::tree::Item::Node(node) => todo!(),
//             }
//         }
//         Ok(())
//     }
// }

// impl TryFrom<&mut Node> for ModuleNode {
//     type Error = Error;

//     fn try_from(node: &mut Node) -> Result<Self, Self::Error> {
//         let mut this = Self::default();
//         assert_eq!(node.root, Keyword::Module);
//         this.id = node.take_left().try_id();
//         while let Some(child_node) = node.take_child() {
//             match &child_node.root {
//                 Keyword::Func => this.functions.push(Function::try_from(child_node)?),
//                 Keyword::Type => this.types.push(FunctionType::try_from(child_node)?),
//                 Keyword::Table => {}
//                 Keyword::Memory => {}
//                 Keyword::Global => {}
//                 Keyword::Elem => {}
//                 Keyword::Data => {}
//                 Keyword::Import => {}
//                 Keyword::Export => {}
//                 Keyword::Start if this.start.is_none() => {}
//                 keyword => {
//                     todo!("Failed to parse")
//                 }
//             }
//         }
//         Ok(this)
//     }
// }

// impl ModuleNode {
//     pub fn validate(self) -> Result<Module, Box<dyn std::error::Error>> {
//         Ok()
//     }
// }
