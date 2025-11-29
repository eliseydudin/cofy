use crate::ast::{SourcePosition, Token, TokenRepr, WithPos};
use bumpalo::{Bump, boxed::Box, collections::Vec, format, vec};
use core::{error, fmt};

pub struct Parser<'bump> {
    bump: &'bump Bump,
    token_stream: &'bump [Token<'bump>],
    current: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operator {
    Mult,
    Div,
    Plus,
    Minus,
    Gt,
    Lt,
    Ge,
    Le,
    Eq,
}

#[derive(Debug, PartialEq)]
pub enum Type<'bump> {
    // T
    Plain(&'bump str),
    // [T]
    Iter(Box<'bump, Self>),
    // (params) => returns
    Function {
        params: Vec<'bump, Self>,
        returns: Box<'bump, Self>,
    },
    // (0, 1, 2)
    Tuple(Vec<'bump, Self>),
    // Ty<A, B, C>
    WithTypeParams(&'bump str, Vec<'bump, Self>),
}

#[derive(Debug, PartialEq)]
pub enum QualImport<'bump> {
    Plain(&'bump str),
    As {
        original: &'bump str,
        after: &'bump str,
    },
}

#[derive(Debug, PartialEq)]
pub enum Import<'bump> {
    Base(&'bump str),
    Access(&'bump str, Box<'bump, Self>),
    Qualified(Vec<'bump, QualImport<'bump>>, Box<'bump, Self>),
}

impl From<TokenRepr> for Operator {
    fn from(value: TokenRepr) -> Self {
        match value {
            TokenRepr::Mult => Operator::Mult,
            TokenRepr::Div => Operator::Div,
            TokenRepr::Plus => Operator::Plus,
            TokenRepr::Minus => Operator::Minus,
            TokenRepr::LAngle => Operator::Lt,
            TokenRepr::RAngle => Operator::Gt,
            TokenRepr::Le => Operator::Le,
            TokenRepr::Ge => Operator::Ge,
            TokenRepr::Equal => Operator::Eq,
            _ => panic!("invalid operator"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Skip,
    Break,
}

#[derive(Debug, PartialEq)]
pub enum ExprRepr<'bump> {
    // 238.39
    Number(&'bump str),
    // bar
    Identifier(&'bump str),
    // "foo"
    String(&'bump str),
    // $
    Pipe,
    // left op right
    BinOp {
        left: Box<'bump, Expr<'bump>>,
        operator: Operator,
        right: Box<'bump, Expr<'bump>>,
    },
    // -obj
    Unary {
        operator: Operator,
        data: Box<'bump, Expr<'bump>>,
    },
    // object.property
    Access {
        object: Box<'bump, Expr<'bump>>,
        property: &'bump str,
    },
    // object(param1, param2)
    Call {
        object: Box<'bump, Expr<'bump>>,
        params: Vec<'bump, Expr<'bump>>,
    },
    // if condition then main_body else else_body
    If {
        condition: Box<'bump, Expr<'bump>>,
        main_body: Box<'bump, Expr<'bump>>,
        else_body: Option<Box<'bump, Expr<'bump>>>,
    },
    // for var in container do action
    For {
        var: &'bump str,
        container: Box<'bump, Expr<'bump>>,
        action: Box<'bump, Expr<'bump>>,
    },
    // (exp1, exp2)
    Tuple(Vec<'bump, Expr<'bump>>),
    // [exp1, exp2]
    List(Vec<'bump, Expr<'bump>>),
    // break or skip
    Keyword(Keyword),

    // object[index]
    IndexAccess {
        object: Box<'bump, Expr<'bump>>,
        index: Box<'bump, Expr<'bump>>,
    },
    // { param1, param2 } -> { body }
    Lambda {
        params: Vec<'bump, Expr<'bump>>,
        body: Box<'bump, Expr<'bump>>,
    },
    // object { field1, field2: value }
    Constructor {
        object: Box<'bump, Expr<'bump>>,
        fields: Vec<'bump, (&'bump str, Option<Expr<'bump>>)>,
    },
}

pub type Expr<'b> = WithPos<ExprRepr<'b>>;

impl<'b> ExprRepr<'b> {
    pub fn binop(bump: &'b Bump, left: Expr<'b>, operator: Operator, right: Expr<'b>) -> Self {
        Self::BinOp {
            left: Box::new_in(left, bump),
            operator,
            right: Box::new_in(right, bump),
        }
    }

    pub fn unary(bump: &'b Bump, operator: Operator, data: Expr<'b>) -> Self {
        Self::Unary {
            operator,
            data: Box::new_in(data, bump),
        }
    }

    pub fn number(data: &'b str) -> Self {
        Self::Number(data)
    }

    pub fn string(data: &'b str) -> Self {
        Self::String(data)
    }

    pub fn identifier(data: &'b str) -> Self {
        Self::Identifier(data)
    }

    pub fn access(bump: &'b Bump, object: Expr<'b>, property: &'b str) -> Self {
        Self::Access {
            object: Box::new_in(object, bump),
            property,
        }
    }

    pub const fn pipe() -> Self {
        Self::Pipe
    }

    pub fn call(bump: &'b Bump, object: Expr<'b>, params: Vec<'b, Expr<'b>>) -> Self {
        Self::Call {
            object: Box::new_in(object, bump),
            params,
        }
    }

    pub fn if_expr(
        bump: &'b Bump,
        condition: Expr<'b>,
        main_body: Expr<'b>,
        else_body: Option<Expr<'b>>,
    ) -> Self {
        Self::If {
            condition: Box::new_in(condition, bump),
            main_body: Box::new_in(main_body, bump),
            else_body: else_body.map(|e| Box::new_in(e, bump)),
        }
    }

    pub fn for_expr(bump: &'b Bump, var: &'b str, container: Expr<'b>, action: Expr<'b>) -> Self {
        Self::For {
            var,
            container: Box::new_in(container, bump),
            action: Box::new_in(action, bump),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParserError<'bump> {
    message: &'bump str,
    position: SourcePosition,
}

impl fmt::Display for ParserError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "at {}: {}", self.position, self.message)
    }
}

impl error::Error for ParserError<'_> {}

pub type ParserResult<'s, T> = Result<T, ParserError<'s>>;

macro_rules! error {
    ($where:expr, $data:expr) => {
        $crate::ast::parser::ParserError {
            message: $data.into(),
            position: $where.pos,
        }
    };
}

impl<'src, 'bump: 'src> Parser<'bump> {
    pub fn new(bump: &'bump Bump, token_stream: &'bump [Token<'src>]) -> Self {
        Self {
            bump,
            token_stream,
            current: 0,
        }
    }

    pub fn expression(&mut self) -> ParserResult<'bump, Expr<'bump>> {
        self.equality()
    }

    pub fn equality(&mut self) -> ParserResult<'bump, Expr<'bump>> {
        let mut expr = self.comparison()?;

        while self.match_next(&[TokenRepr::Equal]) {
            self.current += 1;
            let operator = self
                .previous()
                .expect("after match next we are guaranteed not to go out of bounds");
            let right = self.comparison()?;
            expr = WithPos::new(
                ExprRepr::binop(self.bump, expr, operator.repr.into(), right),
                operator.pos,
            );
        }

        Ok(expr)
    }

    pub fn previous(&self) -> Option<Token<'src>> {
        self.token_stream.get(self.current - 1).copied()
    }

    pub fn peek(&self) -> Option<Token<'src>> {
        self.token_stream.get(self.current).copied()
    }

    pub fn eof(&self) -> bool {
        self.peek().is_none()
    }

    pub fn match_next(&mut self, reprs: &[TokenRepr]) -> bool {
        for r in reprs {
            if self.check(*r) {
                return true;
            }
        }

        false
    }

    pub fn check(&self, repr: TokenRepr) -> bool {
        match self.peek() {
            Some(n) if n.repr == repr => true,
            _ => false,
        }
    }

    pub fn advance(&mut self) -> Option<Token<'src>> {
        if !self.eof() {
            self.current += 1;
        }

        self.previous()
    }

    pub fn comparison(&mut self) -> ParserResult<'bump, Expr<'bump>> {
        let mut expr = self.term()?;

        while self.match_next(&[
            TokenRepr::Ge,
            TokenRepr::RAngle,
            TokenRepr::Le,
            TokenRepr::LAngle,
        ]) {
            self.current += 1;
            let operator = self
                .previous()
                .expect("after match next we are guaranteed not to go out of bounds");
            let right = self.term()?;
            expr = WithPos::new(
                ExprRepr::binop(self.bump, expr, operator.repr.into(), right),
                operator.pos,
            );
        }

        Ok(expr)
    }

    pub fn term(&mut self) -> ParserResult<'bump, Expr<'bump>> {
        let mut expr = self.factor()?;

        while self.match_next(&[TokenRepr::Minus, TokenRepr::Plus]) {
            self.advance();
            let operator = self
                .previous()
                .expect("after match next we are guaranteed not to go out of bounds");
            let right = self.factor()?;
            expr = WithPos::new(
                ExprRepr::binop(self.bump, expr, operator.repr.into(), right),
                operator.pos,
            );
        }

        Ok(expr)
    }

    pub fn factor(&mut self) -> ParserResult<'bump, Expr<'bump>> {
        let mut expr = self.unary()?;

        while self.match_next(&[TokenRepr::Div, TokenRepr::Mult]) {
            self.advance();
            let operator = self
                .previous()
                .expect("after match next we are guaranteed not to go out of bounds");
            let right = self.unary()?;
            expr = WithPos::new(
                ExprRepr::binop(self.bump, expr, operator.repr.into(), right),
                operator.pos,
            );
        }

        Ok(expr)
    }

    pub fn unary(&mut self) -> ParserResult<'bump, Expr<'bump>> {
        if self.match_next(&[TokenRepr::Minus]) {
            self.advance();
            let operator = self
                .previous()
                .expect("after match next we are guaranteed not to go out of bounds");
            let right = self.unary()?;
            return Ok(WithPos::new(
                ExprRepr::unary(self.bump, operator.repr.into(), right),
                operator.pos,
            ));
        }

        self.primary(true)
    }

    pub fn consume(&mut self, tok: TokenRepr) -> ParserResult<'bump, Token<'src>> {
        if self.check(tok) {
            Ok(self
                .advance()
                .expect("shouldn't be None since self.check is true"))
        } else {
            let err = match self.peek() {
                Some(last) => error!(
                    last,
                    format!(in self.bump, "expected a {:?}, found a {:?}", tok, last.repr)
                        .into_bump_str()
                ),
                None => self.eof_error(),
            };

            Err(err)
        }
    }

    pub fn eof_error(&self) -> ParserError<'src> {
        error!(self.previous().unwrap(), "expected a token, found eof")
    }

    pub fn primary(&mut self, complex_ident: bool) -> ParserResult<'bump, Expr<'bump>> {
        let tok = self.peek().ok_or_else(|| self.eof_error())?;
        self.advance().ok_or_else(|| self.eof_error())?;

        match tok.repr {
            TokenRepr::Number => Ok(Expr {
                pos: tok.pos,
                inner: ExprRepr::number(tok.data),
            }),
            TokenRepr::String => Ok(Expr {
                pos: tok.pos,
                inner: ExprRepr::string(tok.data),
            }),
            TokenRepr::Pipe => Ok(Expr {
                pos: tok.pos,
                inner: ExprRepr::Pipe,
            }),
            TokenRepr::LParen => {
                self.current -= 1;
                let mut exp = self.parse_tuple()?;
                if exp.len() == 1 {
                    Ok(exp.remove(0))
                } else {
                    Ok(Expr {
                        pos: tok.pos,
                        inner: ExprRepr::Tuple(exp),
                    })
                }
            }
            TokenRepr::LBracket => {
                self.current -= 1;
                let exp = self.parse_tuple_with(
                    TokenRepr::LBracket,
                    Self::expression,
                    TokenRepr::Coma,
                    TokenRepr::RBracket,
                )?;
                Ok(Expr {
                    pos: tok.pos,
                    inner: ExprRepr::List(exp),
                })
            }
            TokenRepr::Identifier => {
                let start = WithPos::new(ExprRepr::identifier(tok.data), tok.pos);
                if complex_ident {
                    self.current -= 1;
                    self.parse_identifier_or_call(start, None)
                } else {
                    Ok(start)
                }
            }
            TokenRepr::If => {
                self.current -= 1;
                self.parse_if_expr()
            }
            TokenRepr::For => {
                self.current -= 1;
                self.parse_for_expr()
            }
            TokenRepr::Skip => Ok(Expr {
                pos: tok.pos,
                inner: ExprRepr::Keyword(Keyword::Skip),
            }),
            TokenRepr::Break => Ok(Expr {
                pos: tok.pos,
                inner: ExprRepr::Keyword(Keyword::Break),
            }),
            TokenRepr::LFigure => {
                self.current -= 1;
                let params = self.parse_tuple_with(
                    TokenRepr::LFigure,
                    Self::expression,
                    TokenRepr::Coma,
                    TokenRepr::RFigure,
                )?;
                self.consume(TokenRepr::Arrow)?;
                self.consume(TokenRepr::LFigure)?;
                let body = Box::new_in(self.expression()?, self.bump);
                self.consume(TokenRepr::RFigure)?;

                Ok(Expr {
                    pos: tok.pos,
                    inner: ExprRepr::Lambda { params, body },
                })
            }
            _ => todo!("on {:?}", tok.repr),
        }
    }

    pub fn parse_const(&mut self) -> ParserResult<'bump, Ast<'bump>> {
        let begin = self.consume(TokenRepr::Const)?;
        let name = self.consume(TokenRepr::Identifier)?;
        self.consume(TokenRepr::Set)?;
        let value = self.expression()?;
        self.consume(TokenRepr::Semicolon)?;

        Ok(Ast {
            inner: AstInner::Const {
                name: name.data,
                value,
            },
            pos: begin.pos,
        })
    }

    fn parse_type(&mut self) -> ParserResult<'bump, Type<'bump>> {
        let tok = self.advance().ok_or_else(|| self.eof_error())?;
        match tok.repr {
            TokenRepr::Identifier => {
                if self.check(TokenRepr::LAngle) {
                    let type_params = self.parse_type_params()?;
                    Ok(Type::WithTypeParams(tok.data, type_params))
                } else {
                    Ok(Type::Plain(tok.data))
                }
            }
            TokenRepr::LParen => {
                self.current -= 1;
                let params = self.parse_type_tuple()?;

                if self.check(TokenRepr::FatArrow) {
                    self.consume(TokenRepr::FatArrow)?;
                    let returns = Box::new_in(self.parse_type()?, self.bump);
                    Ok(Type::Function { params, returns })
                } else {
                    Ok(Type::Tuple(params))
                }
            }
            TokenRepr::LBracket => {
                let inner = self.parse_type()?;
                self.consume(TokenRepr::RBracket)?;
                Ok(Type::Iter(Box::new_in(inner, self.bump)))
            }
            _ => Err(error!(tok, "unexpected token")),
        }
    }

    fn parse_type_tuple(&mut self) -> ParserResult<'bump, Vec<'bump, Type<'bump>>> {
        self.parse_tuple_with(
            TokenRepr::LParen,
            Self::parse_type,
            TokenRepr::Coma,
            TokenRepr::RParen,
        )
    }

    fn parse_function_type(&mut self) -> ParserResult<'bump, Type<'bump>> {
        let params = self.parse_type_tuple()?;
        self.consume(TokenRepr::FatArrow)?;
        let returns = self.parse_type()?;

        Ok(Type::Function {
            params,
            returns: Box::new_in(returns, self.bump),
        })
    }

    fn parse_function_params(&mut self) -> ParserResult<'bump, Vec<'bump, Expr<'bump>>> {
        let mut result = vec![in self.bump];

        while self.peek().ok_or_else(|| self.eof_error())?.repr != TokenRepr::Set {
            result.push(self.primary(false)?)
        }

        Ok(result)
    }

    fn parse_function_body(&mut self) -> ParserResult<'bump, Vec<'bump, Expr<'bump>>> {
        let mut result = vec![in self.bump];

        loop {
            let expr = self.expression()?;
            result.push(expr);

            let next = self.advance().ok_or_else(|| self.eof_error())?;
            match next.repr {
                TokenRepr::FatArrow => continue,
                TokenRepr::Semicolon => break,
                _ => {
                    return Err(error!(
                        next,
                        format!(in self.bump, "unexpected token of type {:?}", next.repr)
                            .into_bump_str()
                    ));
                }
            }
        }

        Ok(result)
    }

    fn parse_tuple(&mut self) -> ParserResult<'bump, Vec<'bump, Expr<'bump>>> {
        self.parse_tuple_with(
            TokenRepr::LParen,
            Self::expression,
            TokenRepr::Coma,
            TokenRepr::RParen,
        )
    }

    fn parse_constructor_field(
        &mut self,
    ) -> ParserResult<'bump, (&'bump str, Option<Expr<'bump>>)> {
        let field = self.consume(TokenRepr::Identifier)?;

        let data = if self.check(TokenRepr::Colon) {
            self.consume(TokenRepr::Colon)?;
            Some(self.expression()?)
        } else {
            None
        };

        Ok((field.data, data))
    }

    fn parse_identifier_or_call(
        &mut self,
        start: Expr<'bump>,
        type_params: Option<Vec<'bump, Type<'bump>>>,
    ) -> ParserResult<'bump, Expr<'bump>> {
        self.advance().ok_or_else(|| self.eof_error())?;
        let next = self.peek().ok_or_else(|| self.eof_error())?;

        if next.repr == TokenRepr::Dot {
            let property = self.peek().ok_or_else(|| self.eof_error())?;
            let access = Expr {
                pos: start.pos,
                inner: ExprRepr::access(self.bump, start, property.data),
            };

            self.advance().ok_or_else(|| self.eof_error())?;
            self.advance().ok_or_else(|| self.eof_error())?;

            self.parse_identifier_or_call(access, type_params)
        } else if next.repr == TokenRepr::LParen {
            let params = self.parse_tuple()?;

            let function_call = Expr {
                pos: start.pos,
                inner: ExprRepr::call(self.bump, start, params),
            };
            Ok(function_call)
        } else if next.repr == TokenRepr::LAngle && type_params.is_none() {
            // very bad code but idk how to parse it in any other way
            let save = self.current;
            match self.parse_type_params() {
                Ok(data) => {
                    self.current -= 1;
                    self.parse_identifier_or_call(start, Some(data))
                }
                Err(_) => {
                    self.current = save;
                    Ok(start)
                }
            }
        } else if next.repr == TokenRepr::LFigure {
            let fields = self.parse_tuple_with(
                TokenRepr::LFigure,
                Self::parse_constructor_field,
                TokenRepr::Coma,
                TokenRepr::RFigure,
            )?;

            let next = Expr {
                pos: start.pos,
                inner: ExprRepr::Constructor {
                    object: Box::new_in(start, self.bump),
                    fields,
                },
            };

            Ok(next)
        } else if next.repr == TokenRepr::LBracket {
            let b_start = self.consume(TokenRepr::LBracket)?;
            let index = self.expression()?;
            self.consume(TokenRepr::RBracket)?;
            self.current -= 1;

            let next = Expr {
                pos: b_start.pos,
                inner: ExprRepr::IndexAccess {
                    object: Box::new_in(start, self.bump),
                    index: Box::new_in(index, self.bump),
                },
            };

            self.parse_identifier_or_call(next, type_params)
        } else {
            Ok(start)
        }
    }

    fn parse_type_params(&mut self) -> ParserResult<'bump, Vec<'bump, Type<'bump>>> {
        self.parse_tuple_with(
            TokenRepr::LAngle,
            Self::parse_type,
            TokenRepr::Coma,
            TokenRepr::RAngle,
        )
    }

    fn parse_tuple_with<T, F>(
        &mut self,
        start: TokenRepr,
        mut elem_fn: F,
        delimeter: TokenRepr,
        end: TokenRepr,
    ) -> ParserResult<'bump, Vec<'bump, T>>
    where
        F: FnMut(&mut Self) -> ParserResult<'bump, T>,
    {
        let mut result = vec![in self.bump];
        self.consume(start)?;

        loop {
            if self.check(end) {
                self.consume(end)?;
                break;
            }

            let expr = elem_fn(self)?;
            result.push(expr);

            let next = self.advance().ok_or_else(|| self.eof_error())?;

            if next.repr == delimeter {
                continue;
            } else if next.repr == end {
                break;
            } else {
                return Err(error!(
                    next,
                    format!(in self.bump, "unexpected token of type {:?}", next.repr)
                        .into_bump_str()
                ));
            }
        }

        Ok(result)
    }

    pub fn parse_function(&mut self) -> ParserResult<'bump, Ast<'bump>> {
        let begin = self.consume(TokenRepr::Fn)?;
        let name = self.consume(TokenRepr::Identifier)?;

        let type_parameters = if self.check(TokenRepr::LAngle) {
            self.parse_type_params()?
        } else {
            vec![in self.bump]
        };

        if self.check(TokenRepr::LParen) {
            let type_of = self.parse_function_type()?;
            self.consume(TokenRepr::Semicolon)?;
            Ok(Ast {
                inner: AstInner::FunctionPrototype {
                    name: name.data,
                    type_of,
                    type_parameters,
                },
                pos: begin.pos,
            })
        } else {
            let params = self.parse_function_params()?;
            self.consume(TokenRepr::Set)?;

            let with_type = if self.check(TokenRepr::With) {
                self.consume(TokenRepr::With)?;
                let with = Some(self.parse_type()?);
                self.consume(TokenRepr::FatArrow)?;
                with
            } else {
                None
            };

            let body = self.parse_function_body()?;

            Ok(Ast {
                inner: AstInner::Function {
                    name: name.data,
                    params,
                    type_parameters,
                    body,
                    with_type,
                },
                pos: begin.pos,
            })
        }
    }

    pub fn parse_alias(&mut self) -> ParserResult<'bump, Ast<'bump>> {
        let start = self.consume(TokenRepr::Alias)?;
        let name = self.consume(TokenRepr::Identifier)?.data;

        let type_parameters = if self.check(TokenRepr::LAngle) {
            self.parse_type_params()?
        } else {
            vec![in self.bump]
        };

        self.consume(TokenRepr::Set)?;

        let aliasing = self.parse_type()?;
        self.consume(TokenRepr::Semicolon)?;

        Ok(Ast {
            pos: start.pos,
            inner: AstInner::Alias {
                name,
                type_parameters,
                aliasing,
            },
        })
    }

    pub fn synchronize(&mut self) {
        const STATEMENT_STARTS: &[TokenRepr] = &[
            TokenRepr::Const,
            TokenRepr::Fn,
            TokenRepr::Alias,
            TokenRepr::Type,
            TokenRepr::Trait,
            TokenRepr::Data,
            TokenRepr::Impl,
            TokenRepr::Export,
            TokenRepr::Import,
        ];

        loop {
            match self.peek() {
                Some(tok) if STATEMENT_STARTS.contains(&tok.repr) => break,
                None => break,
                _ => {
                    self.advance();
                }
            }
        }
    }

    fn parse_if_expr(&mut self) -> ParserResult<'bump, Expr<'bump>> {
        let start = self.consume(TokenRepr::If)?;
        let condition = self.equality()?;

        self.consume(TokenRepr::Then)?;
        let main_body = self.expression()?;

        let else_body = if self.check(TokenRepr::Else) {
            self.consume(TokenRepr::Else)?;
            Some(self.expression()?)
        } else {
            None
        };

        Ok(Expr {
            pos: start.pos,
            inner: ExprRepr::if_expr(self.bump, condition, main_body, else_body),
        })
    }

    fn parse_for_expr(&mut self) -> ParserResult<'bump, Expr<'bump>> {
        let start = self.consume(TokenRepr::For)?;
        let var = self.consume(TokenRepr::Identifier)?;
        self.consume(TokenRepr::In)?;
        let container = self.expression()?;
        self.consume(TokenRepr::Do)?;
        let action = self.expression()?;

        Ok(Expr {
            pos: start.pos,
            inner: ExprRepr::for_expr(self.bump, var.data, container, action),
        })
    }

    fn parse_type_field(&mut self) -> ParserResult<'bump, (&'bump str, Type<'bump>)> {
        let name = self.consume(TokenRepr::Identifier)?;
        self.consume(TokenRepr::Colon)?;
        let value = self.parse_type()?;

        Ok((name.data, value))
    }

    pub fn parse_type_decl(&mut self) -> ParserResult<'bump, Ast<'bump>> {
        self.consume(TokenRepr::Type)?;
        let name = self.consume(TokenRepr::Identifier)?;

        let type_parameters = if self.check(TokenRepr::LAngle) {
            self.parse_type_params()?
        } else {
            vec![in self.bump]
        };

        self.consume(TokenRepr::Set)?;
        let save = self.current;
        self.consume(TokenRepr::LFigure)?;

        let fields = if self.check(TokenRepr::Elipsis) {
            self.consume(TokenRepr::Elipsis)?;
            self.consume(TokenRepr::RFigure)?;
            None
        } else {
            self.current = save;
            Some(self.parse_tuple_with(
                TokenRepr::LFigure,
                Self::parse_type_field,
                TokenRepr::Coma,
                TokenRepr::RFigure,
            )?)
        };

        self.consume(TokenRepr::Semicolon)?;

        Ok(Ast {
            pos: name.pos,
            inner: AstInner::Type {
                name: name.data,
                type_parameters,
                fields,
            },
        })
    }

    pub fn parse_trait(&mut self) -> ParserResult<'bump, Ast<'bump>> {
        let start = self.consume(TokenRepr::Trait)?;
        let name = self.consume(TokenRepr::Identifier)?;

        let with = if self.check(TokenRepr::With) {
            self.parse_tuple_with(
                TokenRepr::With,
                Self::parse_type,
                TokenRepr::Coma,
                TokenRepr::Set,
            )?
        } else {
            self.consume(TokenRepr::Set)?;
            vec![in self.bump]
        };

        let mut prototypes = vec![in self.bump];

        self.consume(TokenRepr::LFigure)?;
        loop {
            let curr = self.peek().ok_or_else(|| self.eof_error())?;
            if curr.repr == TokenRepr::RFigure {
                self.consume(TokenRepr::RFigure)?;
                break;
            }

            let func = self.parse_function()?;
            prototypes.push(func)
        }

        self.consume(TokenRepr::Semicolon)?;

        Ok(Ast {
            pos: start.pos,
            inner: AstInner::Trait {
                name: name.data,
                with,
                prototypes,
            },
        })
    }

    fn parse_data_variants(&mut self) -> ParserResult<'bump, (&'bump str, Option<Type<'bump>>)> {
        let name = self.consume(TokenRepr::Identifier)?;

        let ty = if self.check(TokenRepr::LParen) {
            let tuple = self.parse_type_tuple()?;
            Some(Type::Tuple(tuple))
        } else {
            None
        };

        Ok((name.data, ty))
    }

    pub fn parse_data(&mut self) -> ParserResult<'bump, Ast<'bump>> {
        let start = self.consume(TokenRepr::Data)?;
        let name = self.consume(TokenRepr::Identifier)?;

        let type_parameters = if self.check(TokenRepr::LAngle) {
            self.parse_type_params()?
        } else {
            vec![in self.bump]
        };

        let variants = self.parse_tuple_with(
            TokenRepr::Set,
            Self::parse_data_variants,
            TokenRepr::BitOr,
            TokenRepr::Semicolon,
        )?;

        Ok(Ast {
            pos: start.pos,
            inner: AstInner::Data {
                name: name.data,
                type_parameters,
                variants,
            },
        })
    }

    pub fn parse_impl(&mut self) -> ParserResult<'bump, Ast<'bump>> {
        let start = self.consume(TokenRepr::Impl)?;
        let impl_trait = self.parse_type()?;
        self.consume(TokenRepr::For)?;
        let impl_for = self.parse_type()?;

        self.consume(TokenRepr::Set)?;
        let definitions = self.parse_tuple_with(
            TokenRepr::LFigure,
            |parser| {
                let next = parser.parse_function();
                if next.is_ok() {
                    // needed since when a function is parsed successfully
                    // it also consumes a `;`, since `;` is used as the delimeter
                    // here it should always be available for consumption by the parser
                    parser.current -= 1;
                }
                next
            },
            TokenRepr::Semicolon,
            TokenRepr::RFigure,
        )?;

        self.consume(TokenRepr::Semicolon)?;

        Ok(Ast {
            pos: start.pos,
            inner: AstInner::Impl {
                impl_for,
                impl_trait,
                definitions,
            },
        })
    }

    pub fn parse_export(&mut self) -> ParserResult<'bump, Ast<'bump>> {
        let start = self.consume(TokenRepr::Export)?;
        let fields = self.parse_tuple_with(
            TokenRepr::LFigure,
            Self::parse_constructor_field,
            TokenRepr::Coma,
            TokenRepr::RFigure,
        )?;
        self.consume(TokenRepr::Semicolon)?;

        Ok(Ast {
            pos: start.pos,
            inner: AstInner::Export { fields },
        })
    }

    fn parse_qualified_import(
        &mut self,
        base: Import<'bump>,
    ) -> ParserResult<'bump, Import<'bump>> {
        let fields = self.parse_tuple_with(
            TokenRepr::LFigure,
            |parser| {
                let field = parser.consume(TokenRepr::Identifier)?;
                let next = parser.peek().ok_or_else(|| parser.eof_error())?;

                if next.repr == TokenRepr::As {
                    parser.consume(TokenRepr::As)?;
                    let after = parser.consume(TokenRepr::Identifier)?;
                    Ok(QualImport::As {
                        original: field.data,
                        after: after.data,
                    })
                } else {
                    Ok(QualImport::Plain(field.data))
                }
            },
            TokenRepr::Coma,
            TokenRepr::RFigure,
        )?;
        Ok(Import::Qualified(fields, Box::new_in(base, self.bump)))
    }

    pub fn parse_import(&mut self) -> ParserResult<'bump, Ast<'bump>> {
        let start = self.consume(TokenRepr::Import)?;
        let mut base = Import::Base(self.consume(TokenRepr::Identifier)?.data);

        loop {
            let next = self.advance().ok_or_else(|| self.eof_error())?;
            match next.repr {
                TokenRepr::Dot => {
                    let field = self.consume(TokenRepr::Identifier)?;
                    base = Import::Access(field.data, Box::new_in(base, self.bump))
                }
                TokenRepr::Semicolon => break,
                TokenRepr::LFigure => {
                    self.current -= 1;
                    base = self.parse_qualified_import(base)?;
                    self.consume(TokenRepr::Semicolon)?;
                    break;
                }
                _ => return Err(error!(next, "unexpected token")),
            }
        }

        Ok(Ast {
            pos: start.pos,
            inner: AstInner::Import(base),
        })
    }
}

#[derive(Debug, PartialEq)]
pub enum AstInner<'bump> {
    Const {
        name: &'bump str,
        value: Expr<'bump>,
    },
    FunctionPrototype {
        name: &'bump str,
        type_of: Type<'bump>,
        type_parameters: Vec<'bump, Type<'bump>>,
    },
    Function {
        name: &'bump str,
        with_type: Option<Type<'bump>>,
        params: Vec<'bump, Expr<'bump>>,
        body: Vec<'bump, Expr<'bump>>,
        type_parameters: Vec<'bump, Type<'bump>>,
    },
    Alias {
        name: &'bump str,
        type_parameters: Vec<'bump, Type<'bump>>,
        aliasing: Type<'bump>,
    },
    Type {
        name: &'bump str,
        type_parameters: Vec<'bump, Type<'bump>>,
        fields: Option<Vec<'bump, (&'bump str, Type<'bump>)>>,
    },
    Trait {
        name: &'bump str,
        with: Vec<'bump, Type<'bump>>,
        prototypes: Vec<'bump, Ast<'bump>>,
    },
    Data {
        name: &'bump str,
        type_parameters: Vec<'bump, Type<'bump>>,
        variants: Vec<'bump, (&'bump str, Option<Type<'bump>>)>,
    },
    Impl {
        impl_for: Type<'bump>,
        impl_trait: Type<'bump>,
        definitions: Vec<'bump, Ast<'bump>>,
    },
    Export {
        fields: Vec<'bump, (&'bump str, Option<Expr<'bump>>)>,
    },
    Import(Import<'bump>),
}

#[derive(Debug, PartialEq)]
pub struct Ast<'bump> {
    pub inner: AstInner<'bump>,
    pub pos: SourcePosition,
}

impl<'src, 'bump: 'src> Iterator for Parser<'bump> {
    type Item = ParserResult<'bump, Ast<'bump>>;

    fn next(&mut self) -> Option<Self::Item> {
        let prev = self.peek()?;
        match prev.repr {
            TokenRepr::Const => Some(self.parse_const()),
            TokenRepr::Fn => Some(self.parse_function()),
            TokenRepr::Alias => Some(self.parse_alias()),
            TokenRepr::Type => Some(self.parse_type_decl()),
            TokenRepr::Trait => Some(self.parse_trait()),
            TokenRepr::Data => Some(self.parse_data()),
            TokenRepr::Impl => Some(self.parse_impl()),
            TokenRepr::Export => Some(self.parse_export()),
            TokenRepr::Import => Some(self.parse_import()),
            _ => {
                let error = error!(
                    prev,
                    format!(in self.bump, "unrecognized statement start {:?}", prev)
                        .into_bump_str()
                );

                self.synchronize();
                Some(Err(error))
            }
        }
    }
}
