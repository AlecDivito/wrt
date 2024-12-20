use std::{io::Write, iter::Peekable};

use crate::{
    execution::{instance::Value, Number},
    lex::{
        ast::{read_number, read_u32, Expect, TryGet},
        Keyword, Token, TokenType,
    },
    structure::types::Index,
};

use super::{
    error::Error,
    model::{
        BinaryOp, Export, Frame, Function, FunctionType, Id, Local, Module, NumberLiteral,
        Parameter, ResultNode, Str, VariableOp,
    },
    types::{NumberType, ValueType},
};

pub trait NodeVisitor: std::fmt::Debug {
    fn visit_module(&mut self, item: &Module) -> Result<(), Error> {
        unimplemented!("Visitor {:?} failed to visit {:?}", self, item)
    }

    fn visit_value_type(&mut self, item: &ValueType) -> Result<(), Error> {
        unimplemented!("Visitor {:?} failed to visit {:?}", self, item)
    }

    fn visit_type(&mut self, item: &FunctionType) -> Result<(), Error> {
        unimplemented!("Visitor {:?} failed to visit {:?}", self, item)
    }

    fn visit_func(&mut self, item: &Function) -> Result<(), Error> {
        unimplemented!("Visitor {:?} failed to visit {:?}", self, item)
    }

    fn visit_param(&mut self, item: &Parameter) -> Result<(), Error> {
        unimplemented!("Visitor {:?} failed to visit {:?}", self, item)
    }

    fn visit_result(&mut self, item: &ResultNode) -> Result<(), Error> {
        unimplemented!("Visitor {:?} failed to visit {:?}", self, item)
    }

    fn visit_local(&mut self, item: &Local) -> Result<(), Error> {
        unimplemented!("Visitor {:?} failed to visit {:?}", self, item)
    }

    fn visit_export(&mut self, item: &Export) -> Result<(), Error> {
        unimplemented!("Visitor {:?} failed to visit {:?}", self, item)
    }

    fn visit_expression(&mut self, item: &Expression) -> Result<(), Error> {
        unimplemented!("Visitor {:?} failed to visit {:?}", self, item)
    }

    fn visit_variable_op(&mut self, item: &VariableOp) -> Result<(), Error> {
        unimplemented!("Visitor {:?} failed to visit {:?}", self, item)
    }

    fn visit_number_binary_op(&mut self, item: &BinaryOp) -> Result<(), Error> {
        unimplemented!("Visitor {:?} failed to visit {:?}", self, item)
    }

    fn visit_number_literal(&mut self, item: &NumberLiteral) -> Result<(), Error> {
        unimplemented!("Visitor {:?} failed to visit {:?}", self, item)
    }

    fn visit_id(&mut self, item: &Id) -> Result<(), Error> {
        unimplemented!("Visitor {:?} failed to visit {:?}", self, item)
    }

    fn visit_string(&mut self, item: &Str) -> Result<(), Error> {
        unimplemented!("Visitor {:?} failed to visit {:?}", self, item)
    }
}

pub trait Accept<V: NodeVisitor> {
    fn accept(&self, v: &mut V) -> Result<(), Error>;
}

#[derive(Debug, Clone)]
pub struct Node {
    pub token: Token,
    pub root: Keyword,
    children: Vec<Item>,
}

impl Node {
    fn new(token: Token, root: Keyword) -> Self {
        Node {
            token,
            root,
            children: vec![],
        }
    }
}

#[derive(Debug, Clone)]
pub enum Item {
    Number(Token),
    String(Token),
    Id(Token),
    Keyword((Keyword, Token)),
    Node(Node),
}

impl Node {
    pub fn expect_keyword(&self, ty: Keyword) -> Result<(), Error> {
        if self.root == ty {
            Ok(())
        } else {
            todo!();
            // Err(Error::new(None, format!("Expected keyword {:?}", ty)))
        }
    }

    pub fn children(&self) -> &[Item] {
        &self.children
    }

    pub fn root(&self) -> &Keyword {
        &self.root
    }
}

pub enum Instruction {
    Const(NumberType),
    LocalGet,
    Add(NumberType),
}

#[derive(Debug, Clone)]
pub struct Const {
    expected: NumberType,
    value: Option<Number>,
}

impl Const {
    pub fn new(expected: NumberType) -> Self {
        Self {
            expected,
            value: None,
        }
    }

    pub fn expected(&self) -> NumberType {
        self.expected
    }

    pub fn value(&self) -> Option<Number> {
        self.value
    }
}

#[derive(Debug, Default, Clone)]
pub struct LocalGet {
    idx: Option<Index>,
}

impl LocalGet {
    pub fn index(&self) -> Option<&Index> {
        self.idx.as_ref()
    }
}

#[derive(Debug, Default, Clone)]
pub struct Unary {
    arg: Option<Expression>,
}

impl Unary {
    pub fn argument(&self) -> Option<&Expression> {
        self.arg.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct BinaryInstruction {
    expected: NumberType,
    lhs: Option<Expression>,
    rhs: Option<Expression>,
}

impl BinaryInstruction {
    pub fn new(expected: NumberType) -> Self {
        Self {
            expected,
            lhs: None,
            rhs: None,
        }
    }

    pub fn execute(&self, frame: &mut Frame) {
        if let Some(lhs) = self.lhs.as_ref() {
            lhs.execute(frame);
        }
        if let Some(rhs) = self.rhs.as_ref() {
            rhs.execute(frame);
        }

        let v1 = frame.pop();
        let v2 = frame.pop();
        let num = self
            .expected
            .add(*v1.as_number().unwrap(), *v2.as_number().unwrap());
        frame.push(Value::Number(num));
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Constant(Const),
    LocalGet(LocalGet),
    Unary(Box<Unary>),
    Binary(Box<BinaryInstruction>),
}

impl Expression {
    pub fn execute(&self, frame: &mut Frame) {
        match self {
            Expression::Constant(value) => match (value.expected(), value.value()) {
                (NumberType::I32, Some(Number::I32(value))) => {
                    frame.push(Value::Number(Number::I32(value)))
                }
                (NumberType::I64, Some(Number::I64(value))) => {
                    frame.push(Value::Number(Number::I64(value)))
                }
                (NumberType::F32, Some(Number::F32(value))) => {
                    frame.push(Value::Number(Number::F32(value)))
                }
                (NumberType::F64, Some(Number::F64(value))) => {
                    frame.push(Value::Number(Number::F64(value)))
                }
                _ => panic!("Validation failed :( during constant expression"),
            },
            Expression::LocalGet(local_get) => {
                if let Some(index) = local_get.index() {
                    frame.push(frame.local(index))
                }
            }
            Expression::Unary(unary) => match unary.argument() {
                Some(expression) => expression.execute(frame),
                None => todo!(),
            },
            Expression::Binary(binary) => binary.execute(frame),
        }
    }
}

impl NodeVisitor for Expression {
    fn visit_number_literal(&mut self, num: &NumberLiteral) -> Result<(), Error> {
        match self {
            Expression::Constant(ty) => ty.value = Some(read_number(ty.expected, &num.token)?),
            Expression::LocalGet(local_get) => {
                local_get.idx = Some(Index::Index(read_u32(&num.token)?))
            }
            _ => unimplemented!(),
        }
        Ok(())
    }

    fn visit_id(&mut self, item: &Id) -> Result<(), Error> {
        match self {
            Expression::LocalGet(local_get) => local_get.idx = Some(Index::Id(item.0.clone())),
            _ => unimplemented!(),
        };
        Ok(())
    }

    fn visit_expression(&mut self, expression: &Expression) -> Result<(), Error> {
        match self {
            Expression::Binary(bin) => {
                if bin.lhs.is_none() {
                    bin.lhs = Some(expression.clone());
                } else if bin.rhs.is_none() {
                    bin.rhs = Some(expression.clone());
                } else {
                    panic!("You can't set multiple values for binary expressions")
                }
            }
            _ => unimplemented!(),
        };
        Ok(())
    }
}

impl<V: NodeVisitor> Accept<V> for Expression {
    fn accept(&self, v: &mut V) -> Result<(), Error> {
        v.visit_expression(self)
    }
}

impl Default for Expression {
    fn default() -> Self {
        Self::Constant(Const::new(NumberType::I32))
    }
}

pub enum Ast {
    Module(Module),
    Type(FunctionType),
    Func(Function),
    Export(Export),
    Param(Parameter),
    Result(ResultNode), // Number(String),
    Local(Local),
    // Types
    ValueType(ValueType),
    // Instructions
    Expression(Expression),
    // Literal
    NumberLiteral(NumberLiteral),
    Id(Id),
    Str(Str),
}

impl Ast {
    fn visit<V: NodeVisitor>(&self, v: &mut V) -> Result<(), Error> {
        println!("VISITOR: BEFORE: {:?}", v);
        match self {
            // Visit the structure
            Ast::Module(module) => module.accept(v),
            Ast::Type(ty) => ty.accept(v),
            Ast::Func(function) => function.accept(v),
            Ast::Export(export) => export.accept(v),
            Ast::Param(parameter) => parameter.accept(v),
            Ast::Result(result) => result.accept(v),
            Ast::Local(local) => local.accept(v),
            Ast::ValueType(value) => value.accept(v),
            // Visit the expression
            Ast::Expression(op) => op.accept(v),
            // Visit the literals
            Ast::NumberLiteral(lit) => lit.accept(v),
            Ast::Id(lit) => lit.accept(v),
            Ast::Str(str) => str.accept(v),
            // Ast::Number(number) => number.accept(v),
        }?;
        println!("VISITOR: AFTER {:?}", v);
        Ok(())
    }

    pub fn as_module(&self) -> Option<&Module> {
        if let Self::Module(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl From<&Node> for Ast {
    fn from(val: &Node) -> Self {
        match &val.root {
            // Types
            Keyword::I32 => Ast::ValueType(ValueType::Number(NumberType::I32)),
            // Structural keywords we can encounter
            Keyword::Type => Ast::Type(FunctionType::default()),
            Keyword::Func => Ast::Func(Function::default()),
            Keyword::Export => Ast::Export(Export::default()),
            Keyword::Param => Ast::Param(Parameter::default()),
            Keyword::Result => Ast::Result(ResultNode::default()),
            Keyword::Local => Ast::Local(Local::default()),
            Keyword::Module => Ast::Module(Module::default()),
            // Instruction Keywords we can encounter
            Keyword::LocalGet => Ast::Expression(Expression::LocalGet(LocalGet::default())),
            Keyword::Add(ty) => {
                Ast::Expression(Expression::Binary(Box::new(BinaryInstruction::new(*ty))))
            }
            Keyword::Const(ty) => Ast::Expression(Expression::Constant(Const::new(*ty))),
            _ => unimplemented!(),
        }
    }
}

pub fn ast(node: &Node) -> Result<Ast, Error> {
    println!("Processing {:?}", node.root);
    let mut ast_node = node.into();

    let mut iter = node.children().iter();
    while let Some(children) = iter.next() {
        let child = match children {
            Item::Number(token) => Ast::NumberLiteral(NumberLiteral::new(token.clone())),
            Item::String(token) => Ast::Str(Str::new(token.source().to_string())),
            Item::Id(token) => Ast::Id(Id::new(token.source().to_string())),
            Item::Keyword((root, token)) => {
                let mut inline_node = Node::new(token.clone(), root.clone());
                // We need to just match for some flat instructions
                if let Keyword::Const(_) = inline_node.root {
                    inline_node.children = vec![iter.next().unwrap().clone()]
                }
                ast(&inline_node)?
            }
            Item::Node(node) => ast(node)?,
        };
        match &mut ast_node {
            Ast::Module(module) => child.visit(module),
            Ast::Func(function) => child.visit(function),
            Ast::Type(function_type) => child.visit(function_type),
            Ast::Export(export) => child.visit(export),
            Ast::Param(parameter) => child.visit(parameter),
            Ast::Result(result_node) => child.visit(result_node),
            Ast::Local(local) => child.visit(local),
            Ast::Expression(expression) => child.visit(expression),
            _ => unimplemented!(),
        }?;
    }

    Ok(ast_node)
}

pub fn parse<'a, I: Iterator<Item = &'a Token> + Clone>(
    tokens: &mut Peekable<I>,
) -> Result<Node, Error> {
    tokens.next().expect_left_paren()?;
    let token = tokens.next().cloned();
    let root = token.as_ref().expect_keyword()?;

    let mut children = vec![];
    while tokens.peek().copied().try_right_paran().is_none() {
        if tokens.peek().copied().try_left_paran().is_some() {
            children.push(Item::Node(parse(tokens)?));
        } else if let Some(peek) = tokens.peek() {
            children.push(match peek.ty() {
                TokenType::Number => Item::Number(tokens.next().unwrap().clone()),
                TokenType::String => Item::String(tokens.next().unwrap().clone()),
                TokenType::Id => Item::Id(tokens.next().unwrap().clone()),
                TokenType::Keyword(keyword) => {
                    Item::Keyword((keyword.clone(), tokens.next().unwrap().clone()))
                }
                _ => todo!(),
            });
        }
    }

    tokens.next().expect_right_paren()?;

    Ok(Node {
        token: token.unwrap(),
        root,
        children,
    })
}

pub fn print_tee_with_indentation<W: Write>(
    f: &mut W,
    tee: &Node,
    indent: usize,
    debug: bool,
) -> std::io::Result<()> {
    writeln!(
        f,
        "{}{}{:?}",
        "  ".repeat(indent),
        if debug { "root:" } else { "" },
        tee.root
    )?;
    for attr in &tee.children {
        write!(f, "{}", "  ".repeat(indent))?;
        match attr {
            Item::String(token) => writeln!(
                f,
                "{}\"{}\"",
                if debug { "String:" } else { "" },
                token.source()
            )?,
            Item::Number(token) => writeln!(
                f,
                "{}{}",
                if debug { "Number:" } else { "" },
                token.source()
            )?,
            Item::Id(token) => {
                writeln!(f, "{}${}", if debug { "Id:" } else { "" }, token.source())?
            }
            Item::Keyword((keyword, _)) => {
                writeln!(f, "{}{:?}", if debug { "keyword:" } else { "" }, keyword)?;
            }
            Item::Node(node) => {
                writeln!(f)?;
                print_tee_with_indentation(f, node, indent + 1, debug)?
            }
        }
    }

    Ok(())
}
