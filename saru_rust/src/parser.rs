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
    Number(i64),
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
            Some(TokenKind::Number(n)) => Ok((&input[1..], Expr::Number(n.parse().unwrap()))),
            Some(TokenKind::Identifier(name)) => Ok((&input[1..], Expr::Identifier(name.clone()))),
            Some(TokenKind::String(s)) => Ok((&input[1..], Expr::String(s.clone()))),
            Some(TokenKind::True) => Ok((&input[1..], Expr::Boolean(true))),
            Some(TokenKind::False) => Ok((&input[1..], Expr::Boolean(false))),
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

    fn parse_infix<'a>(
        &self,
        input: &'a [TokenKind],
        left: Expr,
        precedence: i32,
    ) -> IResult<&'a [TokenKind], Expr> {
        match input.first() {
            Some(op @ TokenKind::Plus) => {
                let (input, right) = self.parse_expr(&input[1..], precedence)?;
                Ok((
                    input,
                    Expr::Infix(InfixOp::Add, Box::new(left), Box::new(right)),
                ))
            }
            Some(op @ TokenKind::Minus) => {
                let (input, right) = self.parse_expr(&input[1..], precedence)?;
                Ok((
                    input,
                    Expr::Infix(InfixOp::Subtract, Box::new(left), Box::new(right)),
                ))
            }
            Some(op @ TokenKind::Asterisk) => {
                let (input, right) = self.parse_expr(&input[1..], precedence)?;
                Ok((
                    input,
                    Expr::Infix(InfixOp::Multiply, Box::new(left), Box::new(right)),
                ))
            }
            Some(op @ TokenKind::Divide) => {
                let (input, right) = self.parse_expr(&input[1..], precedence)?;
                Ok((
                    input,
                    Expr::Infix(InfixOp::Divide, Box::new(left), Box::new(right)),
                ))
            }
            Some(op @ TokenKind::Equal) => {
                let (input, right) = self.parse_expr(&input[1..], precedence)?;
                Ok((
                    input,
                    Expr::Infix(InfixOp::Equal, Box::new(left), Box::new(right)),
                ))
            }
            _ => Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Tag,
            ))),
        }
    }
}
