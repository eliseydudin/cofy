use bumpalo::Bump;
use cofy::ast::{
    Expr, ExprInner, Lexer, LexerError, Operator, Parser,
    parser::{Ast, AstInner, Import, ParserResult, Type},
};
use ptree::TreeBuilder;
use std::{
    error::Error,
    io::{Error as IoErr, stdin},
    ops::Deref,
};

fn op_to_str(op: Operator) -> &'static str {
    match op {
        Operator::Div => "/",
        Operator::Minus => "-",
        Operator::Mult => "*",
        Operator::Plus => "+",
        Operator::Ge => ">=",
        Operator::Le => "<=",
        Operator::Gt => ">",
        Operator::Lt => "<",
        Operator::Eq => "==",
    }
}

fn print_expr(tree: &mut TreeBuilder, expr: &Expr) {
    match &expr.inner {
        ExprInner::Number(num) => tree.add_empty_child(format!("num {num}")),
        ExprInner::String(s) => tree.add_empty_child(format!("string \"{s}\"")),
        ExprInner::BinOp {
            left,
            operator,
            right,
        } => {
            tree.begin_child("binop".to_owned());
            print_expr(tree, left.deref());
            tree.add_empty_child(format!("op {}", op_to_str(*operator)));
            print_expr(tree, right.deref());
            tree.end_child();
            tree
        }
        ExprInner::Unary { operator, data } => {
            tree.begin_child("unary".to_owned());
            tree.add_empty_child(format!("op {}", op_to_str(*operator)));
            print_expr(tree, data.deref());
            tree.end_child()
        }
        ExprInner::Identifier(ident) => tree.add_empty_child(format!("ident {ident}")),
        ExprInner::Call { object, params } => {
            let mutref = tree
                .begin_child("call".to_owned())
                .begin_child("object".to_owned());
            print_expr(mutref, object);
            mutref.end_child().begin_child("params".to_owned());
            for param in params {
                print_expr(mutref, param)
            }

            tree.end_child().end_child()
        }
        ExprInner::Access { object, property } => {
            let mutref = tree
                .begin_child("access".to_owned())
                .begin_child("object".to_owned());
            print_expr(mutref, object);
            mutref
                .end_child()
                .add_empty_child(format!("property `{property}`"))
                .end_child()
        }
        ExprInner::If {
            condition,
            main_body,
            else_body,
        } => {
            let mutref = tree
                .begin_child("if".to_owned())
                .begin_child("condition".to_owned());
            print_expr(mutref, condition);
            mutref.end_child().begin_child("then".to_owned());
            print_expr(mutref, main_body);
            mutref.end_child();

            match else_body {
                Some(else_body) => {
                    mutref.begin_child("else".to_owned());
                    print_expr(mutref, else_body);
                    mutref.end_child()
                }
                None => mutref,
            }
        }
        ExprInner::For {
            var,
            container,
            action,
        } => {
            tree.begin_child("for".to_owned())
                .add_empty_child(format!("var `{var}`"))
                .begin_child("in".to_owned());
            print_expr(tree, container);
            tree.end_child().begin_child("action".to_owned());
            print_expr(tree, action);
            tree.end_child()
        }
        ExprInner::Pipe => tree.add_empty_child("pipe $".to_owned()),
        ExprInner::Tuple(tuple) => {
            tree.begin_child("tuple".to_owned());
            for elem in tuple {
                print_expr(tree, elem);
            }
            tree.end_child()
        }
        ExprInner::Keyword(key) => tree.add_empty_child(format!("keyword {key:?}")),
        ExprInner::IndexAccess { object, index } => {
            let mutref = tree
                .begin_child("access".to_owned())
                .begin_child("object".to_owned());
            print_expr(mutref, object);
            mutref.end_child().begin_child("index".to_owned());
            print_expr(mutref, index);
            mutref.end_child()
        }
        ExprInner::List(list) => {
            tree.begin_child("list".to_owned());
            for elem in list {
                print_expr(tree, elem);
            }
            tree.end_child()
        }
        ExprInner::Lambda { params, body } => {
            tree.begin_child("lambda function".to_owned())
                .begin_child("params".to_owned());
            for param in params {
                print_expr(tree, param);
            }
            tree.end_child().begin_child("body".to_owned());
            print_expr(tree, body);
            tree.end_child().end_child()
        }
        ExprInner::Constructor { object, fields } => {
            tree.begin_child("constructor".to_owned())
                .begin_child("object".to_owned());
            print_expr(tree, object);
            tree.end_child().begin_child("fields".to_owned());
            for (field, value) in fields {
                tree.begin_child(format!("field `{field}`"));
                match value.as_ref() {
                    Some(val) => {
                        print_expr(tree, val);
                    }
                    None => {
                        tree.add_empty_child("<no value>".to_owned());
                    }
                };
                tree.end_child();
            }
            tree
        }
    };
}

fn print_type(tree: &mut TreeBuilder, ty: &Type) {
    match ty {
        Type::Plain(name) => {
            tree.add_empty_child(format!("plain `{name}`"));
        }
        Type::Function { params, returns } => {
            tree.begin_child("fn".to_owned())
                .begin_child("params".to_owned());
            for param in params {
                print_type(tree, param);
            }
            tree.end_child().begin_child("returns".to_owned());
            print_type(tree, returns);
            tree.end_child().end_child();
        }
        Type::Tuple(types) => {
            tree.begin_child("tuple".to_owned());
            for ty in types {
                print_type(tree, ty)
            }
            tree.end_child();
        }
        Type::WithTypeParams(name, params) => {
            tree.begin_child(format!("with params `{name}`"));
            tree.add_empty_child(format!("type params {params:?}"));
            tree.end_child();
        }
        Type::Iter(iter) => {
            tree.begin_child("iter".to_owned());
            print_type(tree, iter);
            tree.end_child();
        }
    };
}

fn print_import(tree: &mut TreeBuilder, import: &Import) {
    match import {
        Import::Base(data) => {
            tree.add_empty_child((*data).to_owned());
        }
        Import::Access(after, before) => {
            tree.begin_child((*after).to_owned());
            print_import(tree, before);
            tree.end_child();
        }
        Import::Qualified(qual, base) => {
            tree.begin_child("qualified".to_owned());
            tree.begin_child("base".to_owned());
            print_import(tree, base);
            tree.end_child();
            for q in qual {
                tree.add_empty_child(format!("{q:?}"));
            }
            tree.end_child();
        }
    }
}

fn print_ast(tree: &mut TreeBuilder, a: ParserResult<Ast>) {
    match a {
        Ok(elem) => match elem.inner {
            AstInner::Const { name, value } => {
                let refmut = tree.begin_child(format!("const `{name}`"));
                print_expr(refmut, &value);
                refmut.end_child();
            }
            AstInner::FunctionPrototype {
                name,
                type_of,
                type_parameters,
            } => {
                let refmut = tree.begin_child(format!("fn proto `{name}`"));
                print_type(refmut, &type_of);
                refmut
                    .add_empty_child(format!("type params {type_parameters:?}"))
                    .end_child();
            }
            AstInner::Function {
                name,
                params,
                body,
                type_parameters,
                with_type,
            } => {
                let refmut = tree
                    .begin_child(format!("fn `{name}`"))
                    .begin_child("params".to_owned());
                for param in params.iter() {
                    print_expr(refmut, param);
                }
                refmut.end_child().begin_child("body".to_owned());
                for pipe in body.iter() {
                    print_expr(refmut, pipe);
                }
                refmut
                    .end_child()
                    .add_empty_child(format!("type params {type_parameters:?}"))
                    .add_empty_child(format!(
                        "with `{}`",
                        with_type
                            .map(|tp| format!("{tp:?}"))
                            .unwrap_or_else(|| "None".to_owned())
                    ))
                    .end_child();
            }
            AstInner::Alias {
                name,
                type_parameters,
                aliasing,
            } => {
                tree.begin_child(format!("alias `{name}`"))
                    .add_empty_child(format!("type params {type_parameters:?}"))
                    .begin_child("aliasing".to_owned());
                print_type(tree, &aliasing);
                tree.end_child().end_child();
            }
            AstInner::Type {
                name,
                type_parameters,
                fields,
            } => {
                tree.begin_child(format!("type `{name}`"))
                    .add_empty_child(format!("type params {type_parameters:?}"))
                    .begin_child("fields".to_owned());

                match fields {
                    Some(f) => {
                        for (name, ty) in f {
                            tree.add_empty_child(format!("`{name}`: {ty:?}"));
                        }
                    }
                    None => {
                        tree.add_empty_child("...".to_owned());
                    }
                }

                tree.end_child().end_child();
            }
            AstInner::Trait {
                name,
                with,
                prototypes,
            } => {
                tree.begin_child(format!("trait `{name}`"))
                    .add_empty_child(format!("with {with:?}"))
                    .begin_child("prototypes".to_owned());

                for proto in prototypes {
                    print_ast(tree, Ok(proto))
                }

                tree.end_child().end_child();
            }
            AstInner::Data {
                name,
                type_parameters,
                variants,
            } => {
                tree.begin_child(format!("data `{name}`"))
                    .add_empty_child(format!("type params {type_parameters:?}"))
                    .begin_child("variants".to_owned());
                for variant in variants {
                    match variant.1 {
                        Some(ty) => {
                            tree.begin_child(format!("variant `{}`", variant.0));
                            print_type(tree, &ty);
                            tree.end_child();
                        }
                        None => {
                            tree.add_empty_child(format!("variant `{}`", variant.0));
                        }
                    }
                }
                tree.end_child().end_child();
            }
            AstInner::Impl {
                impl_for,
                impl_trait,
                definitions,
            } => {
                tree.begin_child("impl".to_owned())
                    .begin_child("for".to_owned());
                print_type(tree, &impl_for);
                tree.end_child().begin_child("trait".to_owned());
                print_type(tree, &impl_trait);
                tree.end_child().begin_child("definitions".to_owned());
                for definition in definitions {
                    print_ast(tree, Ok(definition))
                }
                tree.end_child().end_child();
            }
            AstInner::Export { fields } => {
                tree.begin_child("export".to_owned());
                for (name, value) in fields {
                    tree.begin_child(format!("field: {name}"));
                    match value {
                        Some(v) => print_expr(tree, &v),
                        None => {
                            tree.add_empty_child("<empty>".to_owned());
                        }
                    };
                    tree.end_child();
                }
                tree.end_child();
            }
            AstInner::Import(import) => {
                tree.begin_child("import".to_owned());
                print_import(tree, &import);
                tree.end_child();
            }
        },
        Err(e) => {
            tree.begin_child("error".to_owned())
                .add_empty_child(e.to_string())
                .end_child();
        }
    };
}

fn print_tree(ast: Parser) {
    let mut tree = TreeBuilder::new("stdin's abstract syntax tree".to_owned());
    for a in ast {
        print_ast(&mut tree, a);
    }

    let tree = tree.build();
    ptree::print_tree(&tree).expect("cannot print the tree")
}

fn main() -> Result<(), Box<dyn Error>> {
    let bump = Bump::new();
    println!("write your code here");
    println!("print `end` to start the parser");
    println!();

    let input = stdin()
        .lines()
        .take_while(|a| match a.as_deref() {
            Ok("end") | Err(_) => false,
            _ => true,
        })
        .collect::<Result<Vec<String>, IoErr>>()?
        .join("\n")
        + "\n";

    println!();

    let lexer = Lexer::new(&input);
    let tokens = lexer.collect::<Result<Vec<_>, LexerError>>()?;
    for token in &tokens {
        println!("TOKEN: {token:?}");
    }

    print_tree(Parser::new(&bump, &tokens));
    println!("bytes allocated on the bump: {}", bump.allocated_bytes());

    Ok(())
}
