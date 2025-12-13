use crate::ast::ExprRepr;

use super::{
    Expr, Lexer, Parser, SourcePosition, Token,
    parser::{AstInner, Type},
};
use bumpalo::Bump;

fn tokens<'s>(source: &'s str) -> Vec<Token<'s>> {
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
                params: &[Type::Plain("uint")],
                returns: &Type::Plain("uint"),
            },
            type_parameters: &[]
        }
    );

    assert_eq!(
        impl_0.inner,
        AstInner::Function {
            name: "fib",
            with_type: None,
            params: &[Expr {
                pos: SourcePosition { line: 2, symbol: 7 },
                inner: ExprRepr::number("0")
            }],
            body: &[Expr {
                pos: SourcePosition {
                    line: 2,
                    symbol: 11
                },
                inner: ExprRepr::number("1")
            }],
            type_parameters: &[]
        }
    );

    assert_eq!(
        impl_1.inner,
        AstInner::Function {
            name: "fib",
            with_type: None,
            params: &[Expr {
                pos: SourcePosition { line: 3, symbol: 7 },
                inner: ExprRepr::number("1")
            }],
            body: &[Expr {
                pos: SourcePosition {
                    line: 3,
                    symbol: 11
                },
                inner: ExprRepr::number("1")
            }],
            type_parameters: &[]
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
                params: &[Type::Plain("T")],
                returns: &Type::Plain("T"),
            },
            type_parameters: &[Type::Plain("T")]
        }
    );

    assert_eq!(
        variant.inner,
        AstInner::Function {
            name: "variant",
            with_type: None,
            params: &[Expr {
                pos: SourcePosition {
                    line: 1,
                    symbol: 14
                },
                inner: ExprRepr::identifier("t")
            }],
            body: &[Expr {
                pos: SourcePosition {
                    line: 1,
                    symbol: 18
                },
                inner: ExprRepr::identifier("t")
            }],
            type_parameters: &[Type::Plain("T")]
        }
    );
}
