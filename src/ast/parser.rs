use crate::ast::{SourcePosition, Token, TokenRepr};
use bumpalo::{Bump, boxed::Box, collections::Vec, format, vec};
use core::{error, fmt};

pub struct Parser<'src, 'bump> {
    bump: &'bump Bump,
    token_stream: &'bump [Token<'src>],
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
pub enum Type<'src, 'bump> {
    // T
    Plain(&'src str),
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
    WithTypeParams(&'src str, Vec<'bump, Type<'src, 'bump>>),
}

#[derive(Debug, PartialEq)]
pub enum QualImport<'src> {
    Plain(&'src str),
    As {
        original: &'src str,
        after: &'src str,
    },
}

#[derive(Debug, PartialEq)]
pub enum Import<'src, 'bump> {
    Base(&'src str),
    Access(&'src str, Box<'bump, Self>),
    Qualified(Vec<'bump, QualImport<'src>>, Box<'bump, Self>),
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
pub enum ExprInner<'src, 'bump> {
    // 238.39
    Number(&'src str),
    // bar
    Identifier(&'src str),
    // "foo"
    String(&'src str),
    // $
    Pipe,
    // left op right
    BinOp {
        left: Box<'bump, Expr<'src, 'bump>>,
        operator: Operator,
        right: Box<'bump, Expr<'src, 'bump>>,
    },
    // -obj
    Unary {
        operator: Operator,
        data: Box<'bump, Expr<'src, 'bump>>,
    },
    // object.property
    Access {
        object: Box<'bump, Expr<'src, 'bump>>,
        property: &'src str,
    },
    // object(param1, param2)
    Call {
        object: Box<'bump, Expr<'src, 'bump>>,
        params: Vec<'bump, Expr<'src, 'bump>>,
    },
    // if condition then main_body else else_body
    If {
        condition: Box<'bump, Expr<'src, 'bump>>,
        main_body: Box<'bump, Expr<'src, 'bump>>,
        else_body: Option<Box<'bump, Expr<'src, 'bump>>>,
    },
    // for var in container do action
    For {
        var: &'src str,
        container: Box<'bump, Expr<'src, 'bump>>,
        action: Box<'bump, Expr<'src, 'bump>>,
    },
    // (exp1, exp2)
    Tuple(Vec<'bump, Expr<'src, 'bump>>),
    // [exp1, exp2]
    List(Vec<'bump, Expr<'src, 'bump>>),
    // break or skip
    Keyword(Keyword),

    // object[index]
    IndexAccess {
        object: Box<'bump, Expr<'src, 'bump>>,
        index: Box<'bump, Expr<'src, 'bump>>,
    },
    // { param1, param2 } -> { body }
    Lambda {
        params: Vec<'bump, Expr<'src, 'bump>>,
        body: Box<'bump, Expr<'src, 'bump>>,
    },
    // object { field1, field2: value }
    Constructor {
        object: Box<'bump, Expr<'src, 'bump>>,
        fields: Vec<'bump, (&'src str, Option<Expr<'src, 'bump>>)>,
    },
}

#[derive(Debug, PartialEq)]
pub struct Expr<'src, 'bump> {
    inner: ExprInner<'src, 'bump>,
    pos: SourcePosition,
}

impl<'src, 'bump> Expr<'src, 'bump> {
    pub fn binop(
        at: SourcePosition,
        bump: &'bump Bump,
        left: Self,
        operator: Operator,
        right: Self,
    ) -> Self {
        Self {
            pos: at,
            inner: ExprInner::BinOp {
                left: Box::new_in(left, bump),
                operator,
                right: Box::new_in(right, bump),
            },
        }
    }

    pub fn unary(
        pos: SourcePosition,
        bump: &'bump Bump,
        operator: Operator,
        data: Expr<'src, 'bump>,
    ) -> Self {
        Self {
            pos,
            inner: ExprInner::Unary {
                operator,
                data: Box::new_in(data, bump),
            },
        }
    }

    pub fn number(pos: SourcePosition, data: &'src str) -> Self {
        Self {
            pos,
            inner: ExprInner::Number(data),
        }
    }

    pub fn string(pos: SourcePosition, data: &'src str) -> Self {
        Self {
            pos,
            inner: ExprInner::String(data),
        }
    }

    pub fn identifier(pos: SourcePosition, data: &'src str) -> Self {
        Self {
            pos,
            inner: ExprInner::Identifier(data),
        }
    }

    pub fn access(
        pos: SourcePosition,
        bump: &'bump Bump,
        object: Self,
        property: &'src str,
    ) -> Self {
        Self {
            pos,
            inner: ExprInner::Access {
                object: Box::new_in(object, bump),
                property,
            },
        }
    }

    pub fn pipe(pos: SourcePosition) -> Self {
        Self {
            pos,
            inner: ExprInner::Pipe,
        }
    }

    pub fn call(
        pos: SourcePosition,
        bump: &'bump Bump,
        object: Self,
        params: Vec<'bump, Self>,
    ) -> Self {
        Self {
            pos,
            inner: ExprInner::Call {
                object: Box::new_in(object, bump),
                params,
            },
        }
    }

    pub fn if_expr(
        pos: SourcePosition,
        bump: &'bump Bump,
        condition: Self,
        main_body: Self,
        else_body: Option<Self>,
    ) -> Self {
        Self {
            pos,
            inner: ExprInner::If {
                condition: Box::new_in(condition, bump),
                main_body: Box::new_in(main_body, bump),
                else_body: else_body.map(|e| Box::new_in(e, bump)),
            },
        }
    }

    pub fn for_expr(
        pos: SourcePosition,
        bump: &'bump Bump,
        var: &'src str,
        container: Self,
        action: Self,
    ) -> Self {
        Self {
            pos,
            inner: ExprInner::For {
                var,
                container: Box::new_in(container, bump),
                action: Box::new_in(action, bump),
            },
        }
    }

    pub fn into_inner(self) -> ExprInner<'src, 'bump> {
        self.inner
    }

    pub const fn inner(&self) -> &ExprInner<'src, 'bump> {
        &self.inner
    }
}

#[derive(Debug, Clone)]
pub struct ParserError<'src> {
    message: &'src str,
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

impl<'src, 'bump: 'src> Parser<'src, 'bump> {
    pub fn new(bump: &'bump Bump, token_stream: &'bump [Token<'src>]) -> Self {
        Self {
            bump,
            token_stream,
            current: 0,
        }
    }

    pub fn expression(&mut self) -> ParserResult<'src, Expr<'src, 'bump>> {
        self.equality()
    }

    pub fn equality(&mut self) -> ParserResult<'src, Expr<'src, 'bump>> {
        let mut expr = self.comparison()?;

        while self.match_next(&[TokenRepr::Equal]) {
            self.current += 1;
            let operator = self
                .previous()
                .expect("after match next we are guaranteed not to go out of bounds");
            let right = self.comparison()?;
            expr = Expr::binop(operator.pos, self.bump, expr, operator.repr.into(), right);
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

    pub fn comparison(&mut self) -> ParserResult<'src, Expr<'src, 'bump>> {
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
            expr = Expr::binop(operator.pos, self.bump, expr, operator.repr.into(), right);
        }

        Ok(expr)
    }

    pub fn term(&mut self) -> ParserResult<'src, Expr<'src, 'bump>> {
        let mut expr = self.factor()?;

        while self.match_next(&[TokenRepr::Minus, TokenRepr::Plus]) {
            self.advance();
            let operator = self
                .previous()
                .expect("after match next we are guaranteed not to go out of bounds");
            let right = self.factor()?;
            expr = Expr::binop(operator.pos, self.bump, expr, operator.repr.into(), right);
        }

        Ok(expr)
    }

    pub fn factor(&mut self) -> ParserResult<'src, Expr<'src, 'bump>> {
        let mut expr = self.unary()?;

        while self.match_next(&[TokenRepr::Div, TokenRepr::Mult]) {
            self.advance();
            let operator = self
                .previous()
                .expect("after match next we are guaranteed not to go out of bounds");
            let right = self.unary()?;
            expr = Expr::binop(operator.pos, self.bump, expr, operator.repr.into(), right);
        }

        Ok(expr)
    }

    pub fn unary(&mut self) -> ParserResult<'src, Expr<'src, 'bump>> {
        if self.match_next(&[TokenRepr::Minus]) {
            self.advance();
            let operator = self
                .previous()
                .expect("after match next we are guaranteed not to go out of bounds");
            let right = self.unary()?;
            return Ok(Expr::unary(
                operator.pos,
                self.bump,
                operator.repr.into(),
                right,
            ));
        }

        self.primary(true)
    }

    pub fn consume(&mut self, tok: TokenRepr) -> ParserResult<'src, Token<'src>> {
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

    pub fn primary(&mut self, complex_ident: bool) -> ParserResult<'src, Expr<'src, 'bump>> {
        let tok = self.peek().ok_or_else(|| self.eof_error())?;
        self.advance().ok_or_else(|| self.eof_error())?;

        match tok.repr {
            TokenRepr::Number => Ok(Expr::number(tok.pos, tok.data)),
            TokenRepr::String => Ok(Expr::string(tok.pos, tok.data)),
            TokenRepr::Pipe => Ok(Expr::pipe(tok.pos)),
            TokenRepr::LParen => {
                self.current -= 1;
                let mut exp = self.parse_tuple()?;
                if exp.len() == 1 {
                    Ok(exp.remove(0))
                } else {
                    Ok(Expr {
                        pos: tok.pos,
                        inner: ExprInner::Tuple(exp),
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
                    inner: ExprInner::List(exp),
                })
            }
            TokenRepr::Identifier => {
                let start = Expr::identifier(tok.pos, tok.data);
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
                inner: ExprInner::Keyword(Keyword::Skip),
            }),
            TokenRepr::Break => Ok(Expr {
                pos: tok.pos,
                inner: ExprInner::Keyword(Keyword::Break),
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
                    inner: ExprInner::Lambda { params, body },
                })
            }
            _ => todo!("on {:?}", tok.repr),
        }
    }

    pub fn parse_const(&mut self) -> ParserResult<'src, Ast<'src, 'bump>> {
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

    fn parse_type(&mut self) -> ParserResult<'src, Type<'src, 'bump>> {
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

    fn parse_type_tuple(&mut self) -> ParserResult<'src, Vec<'bump, Type<'src, 'bump>>> {
        self.parse_tuple_with(
            TokenRepr::LParen,
            Self::parse_type,
            TokenRepr::Coma,
            TokenRepr::RParen,
        )
    }

    fn parse_function_type(&mut self) -> ParserResult<'src, Type<'src, 'bump>> {
        let params = self.parse_type_tuple()?;
        self.consume(TokenRepr::FatArrow)?;
        let returns = self.parse_type()?;

        Ok(Type::Function {
            params,
            returns: Box::new_in(returns, self.bump),
        })
    }

    fn parse_function_params(&mut self) -> ParserResult<'src, Vec<'bump, Expr<'src, 'bump>>> {
        let mut result = vec![in self.bump];

        while self.peek().ok_or_else(|| self.eof_error())?.repr != TokenRepr::Set {
            result.push(self.primary(false)?)
        }

        Ok(result)
    }

    fn parse_function_body(&mut self) -> ParserResult<'src, Vec<'bump, Expr<'src, 'bump>>> {
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

    fn parse_tuple(&mut self) -> ParserResult<'src, Vec<'bump, Expr<'src, 'bump>>> {
        self.parse_tuple_with(
            TokenRepr::LParen,
            Self::expression,
            TokenRepr::Coma,
            TokenRepr::RParen,
        )
    }

    fn parse_constructor_field(
        &mut self,
    ) -> ParserResult<'src, (&'src str, Option<Expr<'src, 'bump>>)> {
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
        start: Expr<'src, 'bump>,
        type_params: Option<Vec<'bump, Type<'src, 'bump>>>,
    ) -> ParserResult<'src, Expr<'src, 'bump>> {
        self.advance().ok_or_else(|| self.eof_error())?;
        let next = self.peek().ok_or_else(|| self.eof_error())?;

        if next.repr == TokenRepr::Dot {
            let property = self.peek().ok_or_else(|| self.eof_error())?;
            let access = Expr::access(start.pos, self.bump, start, property.data);

            self.advance().ok_or_else(|| self.eof_error())?;
            self.advance().ok_or_else(|| self.eof_error())?;

            self.parse_identifier_or_call(access, type_params)
        } else if next.repr == TokenRepr::LParen {
            let params = self.parse_tuple()?;

            let function_call = Expr::call(start.pos, self.bump, start, params);
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
                inner: ExprInner::Constructor {
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
                inner: ExprInner::IndexAccess {
                    object: Box::new_in(start, self.bump),
                    index: Box::new_in(index, self.bump),
                },
            };

            self.parse_identifier_or_call(next, type_params)
        } else {
            Ok(start)
        }
    }

    fn parse_type_params(&mut self) -> ParserResult<'src, Vec<'bump, Type<'src, 'bump>>> {
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
    ) -> ParserResult<'src, Vec<'bump, T>>
    where
        F: FnMut(&mut Self) -> ParserResult<'src, T>,
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

    pub fn parse_function(&mut self) -> ParserResult<'src, Ast<'src, 'bump>> {
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

    pub fn parse_alias(&mut self) -> ParserResult<'src, Ast<'src, 'bump>> {
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

    fn parse_if_expr(&mut self) -> ParserResult<'src, Expr<'src, 'bump>> {
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

        Ok(Expr::if_expr(
            start.pos, self.bump, condition, main_body, else_body,
        ))
    }

    fn parse_for_expr(&mut self) -> ParserResult<'src, Expr<'src, 'bump>> {
        let start = self.consume(TokenRepr::For)?;
        let var = self.consume(TokenRepr::Identifier)?;
        self.consume(TokenRepr::In)?;
        let container = self.expression()?;
        self.consume(TokenRepr::Do)?;
        let action = self.expression()?;

        Ok(Expr::for_expr(
            start.pos, self.bump, var.data, container, action,
        ))
    }

    fn parse_type_field(&mut self) -> ParserResult<'src, (&'src str, Type<'src, 'bump>)> {
        let name = self.consume(TokenRepr::Identifier)?;
        self.consume(TokenRepr::Colon)?;
        let value = self.parse_type()?;

        Ok((name.data, value))
    }

    pub fn parse_type_decl(&mut self) -> ParserResult<'src, Ast<'src, 'bump>> {
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

    pub fn parse_trait(&mut self) -> ParserResult<'src, Ast<'src, 'bump>> {
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

    fn parse_data_variants(
        &mut self,
    ) -> ParserResult<'src, (&'src str, Option<Type<'src, 'bump>>)> {
        let name = self.consume(TokenRepr::Identifier)?;

        let ty = if self.check(TokenRepr::LParen) {
            let tuple = self.parse_type_tuple()?;
            Some(Type::Tuple(tuple))
        } else {
            None
        };

        Ok((name.data, ty))
    }

    pub fn parse_data(&mut self) -> ParserResult<'src, Ast<'src, 'bump>> {
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

    pub fn parse_impl(&mut self) -> ParserResult<'src, Ast<'src, 'bump>> {
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

    pub fn parse_export(&mut self) -> ParserResult<'src, Ast<'src, 'bump>> {
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
        base: Import<'src, 'bump>,
    ) -> ParserResult<'src, Import<'src, 'bump>> {
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

    pub fn parse_import(&mut self) -> ParserResult<'src, Ast<'src, 'bump>> {
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
pub enum AstInner<'src, 'bump> {
    Const {
        name: &'src str,
        value: Expr<'src, 'bump>,
    },
    FunctionPrototype {
        name: &'src str,
        type_of: Type<'src, 'bump>,
        type_parameters: Vec<'bump, Type<'src, 'bump>>,
    },
    Function {
        name: &'src str,
        with_type: Option<Type<'src, 'bump>>,
        params: Vec<'bump, Expr<'src, 'bump>>,
        body: Vec<'bump, Expr<'src, 'bump>>,
        type_parameters: Vec<'bump, Type<'src, 'bump>>,
    },
    Alias {
        name: &'src str,
        type_parameters: Vec<'bump, Type<'src, 'bump>>,
        aliasing: Type<'src, 'bump>,
    },
    Type {
        name: &'src str,
        type_parameters: Vec<'bump, Type<'src, 'bump>>,
        fields: Option<Vec<'bump, (&'src str, Type<'src, 'bump>)>>,
    },
    Trait {
        name: &'src str,
        with: Vec<'bump, Type<'src, 'bump>>,
        prototypes: Vec<'bump, Ast<'src, 'bump>>,
    },
    Data {
        name: &'src str,
        type_parameters: Vec<'bump, Type<'src, 'bump>>,
        variants: Vec<'bump, (&'src str, Option<Type<'src, 'bump>>)>,
    },
    Impl {
        impl_for: Type<'src, 'bump>,
        impl_trait: Type<'src, 'bump>,
        definitions: Vec<'bump, Ast<'src, 'bump>>,
    },
    Export {
        fields: Vec<'bump, (&'src str, Option<Expr<'src, 'bump>>)>,
    },
    Import(Import<'src, 'bump>),
}

#[derive(Debug, PartialEq)]
pub struct Ast<'src, 'bump> {
    pub inner: AstInner<'src, 'bump>,
    pub pos: SourcePosition,
}

impl<'src, 'bump: 'src> Iterator for Parser<'src, 'bump> {
    type Item = ParserResult<'src, Ast<'src, 'bump>>;

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
