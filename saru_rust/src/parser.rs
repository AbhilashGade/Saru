use crate::tokens::TokenKind;
use nom::{
    branch::alt,
    combinator::{map, value},
    sequence::{delimited, preceded, terminated},
    IResult,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Boolean(bool), 
    Identifier(String),
    String(String),
    Prefix(PrefixOp, Box<Expr>),
    Infix(InfixOp, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Vec<Expr>>, Option<Box<Vec<Expr>>>),
    Function(String, Vec<String>, Box<Vec<Expr>>),
    Call(Box<Expr>, Vec<Expr>),
}

#[derive(Debug, Clone)]
pub enum PrefixOp {
    Plus,
    Minus,
    Not,
}

#[derive(Debug, Clone)]
pub enum InfixOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    BitwiseShiftLeft,
    BitwiseShiftRight,
    BitwiseRotateLeft,
    BitwiseRotateRight,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let(String, Expr),
    Return(Option<Expr>),
    Expression(Expr),
}

pub struct Program {
    pub statements: Vec<Statement>,
}

pub struct Parser {
    precedences: HashMap<TokenKind, i32>,
}

impl Parser {
    pub fn new() -> Self {
        let mut precedences = HashMap::new();
        precedences.insert(TokenKind::Equal, 1);
        precedences.insert(TokenKind::LessThan, 2);
        precedences.insert(TokenKind::GreaterThan, 2);
        precedences.insert(TokenKind::LessThanEqual, 2);
        precedences.insert(TokenKind::GreaterThanEqual, 2);
        precedences.insert(TokenKind::Plus, 3);
        precedences.insert(TokenKind::Minus, 3);
        precedences.insert(TokenKind::Asterisk, 4);
        precedences.insert(TokenKind::Divide, 4);
        precedences.insert(TokenKind::BitwiseShiftLeft, 5);
        precedences.insert(TokenKind::BitwiseShiftRight, 5);
        precedences.insert(TokenKind::BitwiseRotateLeft, 5);
        precedences.insert(TokenKind::BitwiseRotateRight, 5);
        Parser { precedences }
    }

    fn get_precedence(&self, token: &TokenKind) -> i32 {
        *self.precedences.get(token).unwrap_or(&0)
    }

        fn match_token(token: TokenKind) -> impl Fn(&[TokenKind]) -> IResult<&[TokenKind], ()> {
        move |input: &[TokenKind]| {
            if input.first().map_or(false, |t| t == &token) {
                Ok((&input[1..], ()))
            } else {
                Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::Tag,
                )))
            }
        }
    }


    pub fn parse_program<'a>(&self, input: &'a [TokenKind]) -> IResult<&'a [TokenKind], Program> {
        let (input, statements) = self.parse_statements(input)?;
        Ok((input, Program { statements }))
    }

    fn parse_statements<'a>(&self, input: &'a [TokenKind]) -> IResult<&'a [TokenKind], Vec<Statement>> {
        let mut statements = Vec::new();
        let mut current_input = input;

        while !current_input.is_empty() {
            let (new_input, stmt) = self.parse_statement(current_input)?;
            statements.push(stmt);
            current_input = new_input;

            if let Some(TokenKind::Semicolon) = current_input.first() {
                current_input = &current_input[1..];
            }
        }

        Ok((current_input, statements))
    }

    fn parse_statement<'a>(&self, input: &'a [TokenKind]) -> IResult<&'a [TokenKind], Statement> {
        match input.first() {
            Some(TokenKind::Let) => self.parse_let_statement(input),
            Some(TokenKind::Return) => self.parse_return_statement(input),
            _ => self.parse_expression_statement(input),
        }
    }

    fn parse_let_statement<'a>(&self, input: &'a [TokenKind]) -> IResult<&'a [TokenKind], Statement> {
        let input = &input[1..]; // Skip 'let'

        let (input, name) = match input.first() {
            Some(TokenKind::Identifier(name)) => (&input[1..], name.clone()),
            _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
        };

        let (input, _) = Self::match_token(TokenKind::Assign)(input)?;
        let (input, expr) = self.parse_expr(input, 0)?;

        Ok((input, Statement::Let(name, expr)))
    }

    fn parse_return_statement<'a>(&self, input: &'a [TokenKind]) -> IResult<&'a [TokenKind], Statement> {
        let input = &input[1..]; // Skip 'return'

        if input.first() == Some(&TokenKind::Semicolon) {
            Ok((&input[1..], Statement::Return(None)))
        } else {
            let (input, expr) = self.parse_expr(input, 0)?;
            Ok((input, Statement::Return(Some(expr))))
        }
    }

    fn parse_expression_statement<'a>(&self, input: &'a [TokenKind]) -> IResult<&'a [TokenKind], Statement> {
        let (input, expr) = self.parse_expr(input, 0)?;
        Ok((input, Statement::Expression(expr)))
    }

    pub fn parse_expr<'a>(
        &self,
        input: &'a [TokenKind],
        min_precedence: i32,
    ) -> IResult<&'a [TokenKind], Expr> {
        let (mut input, mut left) = self.parse_prefix(input)?;

        while let Some(token) = input.first() {
            let precedence = self.get_precedence(token);
            if precedence < min_precedence {
                break;
            }

            let (new_input, new_left) = self.parse_infix(input, left.clone(), precedence)?;
            input = new_input;
            left = new_left;
        }

        Ok((input, left))
    }

    fn parse_prefix<'a>(&self, input: &'a [TokenKind]) -> IResult<&'a [TokenKind], Expr> {
        match input.first() {
            Some(TokenKind::Int(n)) => Ok((&input[1..], Expr::Int(n.parse().unwrap()))),
            Some(TokenKind::Identifier(name)) => Ok((&input[1..], Expr::Identifier(name.clone()))),
            Some(TokenKind::String(s)) => Ok((&input[1..], Expr::String(s.clone()))),
            Some(TokenKind::True) => Ok((&input[1..], Expr::Boolean(true))),
            Some(TokenKind::False) => Ok((&input[1..], Expr::Boolean(false))),
            Some(TokenKind::If) => self.parse_if_expression(input),
            Some(TokenKind::Function) => self.parse_function_expression(input),
            Some(TokenKind::LParen) => {
                let (input, expr) = delimited(
                    value((), |i| Ok((&i[1..], ()))),
                    |i| self.parse_expr(i, 0),
                    value((), |i| Ok((&i[1..], ()))),
                )(input)?;
                Ok((input, expr))
            }
            Some(TokenKind::Minus) => {
                let (input, expr) = self.parse_expr(&input[1..], 5)?;
                Ok((input, Expr::Prefix(PrefixOp::Minus, Box::new(expr))))
            }
            _ => Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Tag,
            ))),
        }
    }

    fn parse_if_expression<'a>(&self, input: &'a [TokenKind]) -> IResult<&'a [TokenKind], Expr> {
        let input = &input[1..]; // Skip 'if'

        let (input, _) = match_token(TokenKind::LParen)(input)?;
        let (input, condition) = self.parse_expr(input, 0)?;
        let (input, _) = match_token(TokenKind::RParen)(input)?;

        let (input, _) = match_token(TokenKind::LBrace)(input)?;
        let (input, consequence) = self.parse_expressions(input, Some(TokenKind::RBrace))?;
        let (mut input, _) = match_token(TokenKind::RBrace)(input)?;

        let alternative = if input.first() == Some(&TokenKind::Else) {
            input = &input[1..];
            let (new_input, _) = match_token(TokenKind::LBrace)(input)?;
            let (new_input, else_block) = self.parse_expressions(new_input, Some(TokenKind::RBrace))?;
            let (new_input, _) = match_token(TokenKind::RBrace)(new_input)?;
            input = new_input;
            Some(Box::new(else_block))
        } else {
            None
        };

        Ok((input, Expr::If(Box::new(condition), Box::new(consequence), alternative)))
    }

    fn parse_function_expression<'a>(&self, input: &'a [TokenKind]) -> IResult<&'a [TokenKind], Expr> {
        let input = &input[1..]; // Skip 'fn'

        let (input, name) = match input.first() {
            Some(TokenKind::Identifier(name)) => (&input[1..], name.clone()),
            _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
        };

        let (input, _) = match_token(TokenKind::LParen)(input)?;
        let (input, params) = self.parse_function_params(input)?;
        let (input, _) = match_token(TokenKind::RParen)(input)?;

        let (input, _) = match_token(TokenKind::LBrace)(input)?;
        let (input, body) = self.parse_expressions(input, Some(TokenKind::RBrace))?;
        let (input, _) = match_token(TokenKind::RBrace)(input)?;

        Ok((input, Expr::Function(name, params, Box::new(body))))
    }

    fn parse_function_params<'a>(&self, input: &'a [TokenKind]) -> IResult<&'a [TokenKind], Vec<String>> {
        let mut params = Vec::new();
        let mut current_input = input;

        while let Some(token) = current_input.first() {
            match token {
                TokenKind::RParen => break,
                TokenKind::Identifier(name) => {
                    params.push(name.clone());
                    current_input = &current_input[1..];
                    
                    match current_input.first() {
                        Some(TokenKind::Comma) => current_input = &current_input[1..],
                        _ => break,
                    }
                }
                _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
            }
        }

        Ok((current_input, params))
    }

    fn parse_infix<'a>(
        &self,
        input: &'a [TokenKind],
        left: Expr,
        precedence: i32,
    ) -> IResult<&'a [TokenKind], Expr> {
        match input.first() {
            Some(_op @ TokenKind::Plus) => {
                let (input, right) = self.parse_expr(&input[1..], precedence)?;
                Ok((input, Expr::Infix(InfixOp::Add, Box::new(left), Box::new(right))))
            }
            Some(_op @ TokenKind::Minus) => {
                let (input, right) = self.parse_expr(&input[1..], precedence)?;
                Ok((input, Expr::Infix(InfixOp::Subtract, Box::new(left), Box::new(right))))
            }
            Some(_op @ TokenKind::Asterisk) => {
                let (input, right) = self.parse_expr(&input[1..], precedence)?;
                Ok((input, Expr::Infix(InfixOp::Multiply, Box::new(left), Box::new(right))))
            }
            Some(_op @ TokenKind::Divide) => {
                let (input, right) = self.parse_expr(&input[1..], precedence)?;
                Ok((input, Expr::Infix(InfixOp::Divide, Box::new(left), Box::new(right))))
            }
            Some(_op @ TokenKind::Equal) => {
                let (input, right) = self.parse_expr(&input[1..], precedence)?;
                Ok((input, Expr::Infix(InfixOp::Equal, Box::new(left), Box::new(right))))
            }
            Some(_op @ TokenKind::LParen) => {
                let (input, args) = self.parse_call_arguments(&input[1..])?;
                Ok((input, Expr::Call(Box::new(left), args)))
            }
            _ => Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Tag,
            ))),
        }
    }

    fn parse_call_arguments<'a>(&self, input: &'a [TokenKind]) -> IResult<&'a [TokenKind], Vec<Expr>> {
        let mut args = Vec::new();
        let mut current_input = input;

        while let Some(token) = current_input.first() {
            match token {
                TokenKind::RParen => {
                    current_input = &current_input[1..];
                    break;
                }
                _ => {
                    let (new_input, arg) = self.parse_expr(current_input, 0)?;
                    args.push(arg);
                    current_input = new_input;

                    match current_input.first() {
                        Some(TokenKind::Comma) => current_input = &current_input[1..],
                        Some(TokenKind::RParen) => {
                            current_input = &current_input[1..];
                            break;
                        }
                        _ => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
                    }
                }
            }
        }

        Ok((current_input, args))
    }

    fn parse_expressions<'a>(
    &self,
    input: &'a [TokenKind],
    end_token: Option<TokenKind>,
) -> IResult<&'a [TokenKind], Vec<Expr>> {
    let mut expressions = Vec::new();
    let mut current_input = input;

    while !current_input.is_empty() {
        // Check for end token if specified
        if let Some(end) = &end_token {
            if current_input.first() == Some(end) {
                break;
            }
        }

        // Parse single expression
        let (new_input, expr) = self.parse_expr(current_input, 0)?;
        expressions.push(expr);

        // Handle semicolon separator
        current_input = match new_input.first() {
            Some(TokenKind::Semicolon) => &new_input[1..],
            _ => new_input,
        };

        // Break if we've reached the end
        if current_input.is_empty() {
            break;
        }
    }

    Ok((current_input, expressions))
}
}
