use bumpalo::Bump;
use cofy::ast::{Expr, ExprInner, Lexer, LexerError, Operator, Parser};
use core::error::Error;
use std::io::{Error as IoErr, ErrorKind, stdin};

fn calculate_expression(expr: &Expr) -> f64 {
    match &expr.inner {
        ExprInner::BinOp {
            left,
            operator,
            right,
        } => {
            let left = calculate_expression(&*left);
            let right = calculate_expression(&*right);

            match operator {
                Operator::Plus => left + right,
                Operator::Minus => left - right,
                Operator::Div => left / right,
                Operator::Mult => left * right,
                Operator::Gt => (left > right) as i32 as f64,
                Operator::Lt => (left < right) as i32 as f64,
                Operator::Ge => (left >= right) as i32 as f64,
                Operator::Le => (left <= right) as i32 as f64,
                Operator::Eq => (left == right) as i32 as f64,
            }
        }
        ExprInner::Number(num) => num.parse().unwrap(),
        ExprInner::Unary { data, .. } => -calculate_expression(&*data),
        _ => todo!("currently unsupported"),
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let bump = Bump::new();
    let input = stdin()
        .lines()
        .next()
        .ok_or(IoErr::new(ErrorKind::Other, "no input"))??;
    let lexer = Lexer::new(&input);
    let tokens = lexer.collect::<Result<Vec<_>, LexerError>>()?;
    let mut parser = Parser::new(&bump, &tokens);

    match parser.expression() {
        Ok(expr) => {
            println!("found the following expression: {expr:#?}");
            let res = calculate_expression(&expr);
            println!("the result of the expression: {res}");
        }
        Err(e) => println!("found the following error: {e}"),
    }

    println!("bytes allocated: {}", bump.allocated_bytes());

    Ok(())
}
