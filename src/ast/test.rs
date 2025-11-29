use crate::ast::ExprRepr;

use super::{
    Expr, Lexer, Parser, SourcePosition, Token,
    parser::{AstInner, Type},
};
use bumpalo::{Bump, boxed::Box, vec};

fn tokens(source: &str) -> Vec<Token> {
    Lexer::new(source)
        .map(|tok| match tok {
            Ok(tok) => tok,
            Err(e) => panic!("while lexing tokens: {e}"),
        })
        .collect::<Vec<Token>>()
}

#[test]
fn fib_test() {
    let bump = Bump::new();
    let source = include_str!("./test/fib.cofy");
    let toks = tokens(source);
    let mut parser = Parser::new(&bump, &toks);

    let prototype = parser.next().unwrap().unwrap();
    let impl_0 = parser.next().unwrap().unwrap();
    let impl_1 = parser.next().unwrap().unwrap();
    let generic = parser.next().unwrap().unwrap();

    assert_eq!(
        prototype.inner,
        AstInner::FunctionPrototype {
            name: "fib",
            type_of: Type::Function {
                params: vec![in &bump; Type::Plain("uint")],
                returns: Box::new_in(Type::Plain("uint"), &bump),
            },
            type_parameters: vec![in &bump]
        }
    );

    assert_eq!(
        impl_0.inner,
        AstInner::Function {
            name: "fib",
            with_type: None,
            params: vec![in &bump;  Expr {pos: SourcePosition { line: 2, symbol: 7 }, inner: ExprRepr::number("0")}],
            body: vec![in &bump; Expr {pos: SourcePosition { line: 2, symbol: 11 }, inner: ExprRepr::number("1")}],
            type_parameters: vec![in &bump]
        }
    );

    assert_eq!(
        impl_1.inner,
        AstInner::Function {
            name: "fib",
            with_type: None,
            params: vec![in &bump; Expr{pos: SourcePosition { line: 3, symbol: 7 }, inner: ExprRepr::number("1")}],
            body: vec![in &bump; Expr{pos: SourcePosition { line: 3, symbol: 11 }, inner: ExprRepr::number("1")}],
            type_parameters: vec![in &bump]
        }
    );

    assert!(matches!(
        generic.inner,
        AstInner::Function {
            name: "fib",
            with_type: None,
            ..
        }
    ));
}

#[test]
fn template_test() {
    let bump = Bump::new();
    let source = include_str!("./test/template.cofy");
    let toks = tokens(source);
    let mut parser = Parser::new(&bump, &toks);

    let prototype = parser.next().unwrap().unwrap();
    let variant = parser.next().unwrap().unwrap();

    assert_eq!(
        prototype.inner,
        AstInner::FunctionPrototype {
            name: "variant",
            type_of: Type::Function {
                params: vec![in &bump; Type::Plain("T")],
                returns: Box::new_in(Type::Plain("T"), &bump),
            },
            type_parameters: vec![in &bump; Type::Plain("T")]
        }
    );

    assert_eq!(
        variant.inner,
        AstInner::Function {
            name: "variant",
            with_type: None,
            params: vec![in &bump; Expr { pos: SourcePosition { line: 1, symbol: 14 }, inner: ExprRepr::identifier("t")}],
            body: vec![in &bump; Expr { pos: SourcePosition { line: 1, symbol: 18 }, inner: ExprRepr::identifier("t")}],
            type_parameters: vec![in &bump; Type::Plain("T")]
        }
    );
}
