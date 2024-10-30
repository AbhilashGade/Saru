#![allow(dead_code)]

use std::{hash::Hash, hash::Hasher};

// Your code goes here
#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
}

// Define the TokenType as an enum
#[derive(Debug, PartialEq, PartialOrd, Hash, Eq)]
pub enum TokenKind {
    Illegal,
    Eof,
    Identifier(String),
    // Data types
    Int(String),
    Float(String),
    String(String),
    True,
    False,

    // Operators
    Assign(String),
    Plus,
    Minus,
    Divide,
    Multiply,
    Asterisk,

    // Delimiters
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function(String),
    Colon,
    Let(String),

    // Keywords
    If,
    Else,
    Return,
    While,
    For,
    In,
    Break,
    Continue,
    Match,
    Case,
    Default,
    Switch,
    Struct,
    Enum,
    Type,
    Module,
    Import,
    Export,
    Const,
    Static,
    Mut,
    Ref,

    // Comparison operators
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    LogicalAnd,
    LogicalOr,
    Increment,
    Decrement,

    // Bitwise operators
    BitwiseShiftLeft,
    BitwiseShiftRight,
    BitwiseRotateLeft,
    BitwiseRotateRight,
    BitwiseAndAssign,
    BitwiseOrAssign,
    BitwiseXorAssign,
    BitwiseShiftLeftAssign,
    BitwiseShiftRightAssign,
    BitwiseRotateLeftAssign,
    BitwiseRotateRightAssign,
}

impl Token {
    pub fn new(kind: TokenKind, value: String) -> Token {
        Token { kind, value }
    }
}
pub fn issue_token(kind: TokenKind, value: String) -> Token {
    match kind {
        TokenKind::Identifier(_) => Token::new(TokenKind::Identifier(value.clone()), value),
        TokenKind::Int(_) => Token::new(TokenKind::Int(value.clone()), value),
        TokenKind::Float(_) => Token::new(TokenKind::Float(value.clone()), value),
        TokenKind::String(_) => Token::new(TokenKind::String(value.clone()), value),
        TokenKind::True => Token::new(TokenKind::True, value),
        TokenKind::False => Token::new(TokenKind::False, value),
        // Operators
        TokenKind::Assign(_) => Token::new(TokenKind::Assign(value.clone()), value),
        TokenKind::Plus => Token::new(TokenKind::Plus, value),
        TokenKind::Minus => Token::new(TokenKind::Minus, value),
        TokenKind::Divide => Token::new(TokenKind::Divide, value),
        TokenKind::Multiply => Token::new(TokenKind::Multiply, value),
        TokenKind::Asterisk => Token::new(TokenKind::Asterisk, value),
        // Delimiters
        TokenKind::Comma => Token::new(TokenKind::Comma, value),
        TokenKind::Semicolon => Token::new(TokenKind::Semicolon, value),
        TokenKind::LParen => Token::new(TokenKind::LParen, value),
        TokenKind::RParen => Token::new(TokenKind::RParen, value),
        TokenKind::LBrace => Token::new(TokenKind::LBrace, value),
        TokenKind::RBrace => Token::new(TokenKind::RBrace, value),
        TokenKind::Function(_) => Token::new(TokenKind::Function(value.clone()), value),
        TokenKind::Colon => Token::new(TokenKind::Colon, value),
        TokenKind::Let(_) => Token::new(TokenKind::Let(value.clone()), value),
        // Keywords
        TokenKind::If => Token::new(TokenKind::If, value),
        TokenKind::Else => Token::new(TokenKind::Else, value),
        TokenKind::Return => Token::new(TokenKind::Return, value),
        TokenKind::While => Token::new(TokenKind::While, value),
        TokenKind::For => Token::new(TokenKind::For, value),
        TokenKind::In => Token::new(TokenKind::In, value),
        TokenKind::Break => Token::new(TokenKind::Break, value),
        TokenKind::Continue => Token::new(TokenKind::Continue, value),
        TokenKind::Match => Token::new(TokenKind::Match, value),
        TokenKind::Case => Token::new(TokenKind::Case, value),
        TokenKind::Default => Token::new(TokenKind::Default, value),
        TokenKind::Switch => Token::new(TokenKind::Switch, value),
        TokenKind::Struct => Token::new(TokenKind::Struct, value),
        TokenKind::Enum => Token::new(TokenKind::Enum, value),
        TokenKind::Type => Token::new(TokenKind::Type, value),
        TokenKind::Module => Token::new(TokenKind::Module, value),
        TokenKind::Import => Token::new(TokenKind::Import, value),
        TokenKind::Export => Token::new(TokenKind::Export, value),
        TokenKind::Const => Token::new(TokenKind::Const, value),
        TokenKind::Static => Token::new(TokenKind::Static, value),
        TokenKind::Mut => Token::new(TokenKind::Mut, value),
        TokenKind::Ref => Token::new(TokenKind::Ref, value),

        // Comparison operators
        TokenKind::Equal => Token::new(TokenKind::Equal, value),
        TokenKind::NotEqual => Token::new(TokenKind::NotEqual, value),
        TokenKind::LessThan => Token::new(TokenKind::LessThan, value),
        TokenKind::GreaterThan => Token::new(TokenKind::GreaterThan, value),
        TokenKind::LessThanEqual => Token::new(TokenKind::LessThanEqual, value),
        TokenKind::GreaterThanEqual => Token::new(TokenKind::GreaterThanEqual, value),
        TokenKind::LogicalAnd => Token::new(TokenKind::LogicalAnd, value),
        TokenKind::LogicalOr => Token::new(TokenKind::LogicalOr, value),
        TokenKind::Increment => Token::new(TokenKind::Increment, value),
        TokenKind::Decrement => Token::new(TokenKind::Decrement, value),
        // Bitwise operators
        TokenKind::BitwiseShiftLeft => Token::new(TokenKind::BitwiseShiftLeft, value),
        TokenKind::BitwiseShiftRight => Token::new(TokenKind::BitwiseShiftRight, value),
        TokenKind::BitwiseRotateLeft => Token::new(TokenKind::BitwiseRotateLeft, value),
        TokenKind::BitwiseRotateRight => Token::new(TokenKind::BitwiseRotateRight, value),
        TokenKind::BitwiseAndAssign => Token::new(TokenKind::BitwiseAndAssign, value),
        TokenKind::BitwiseOrAssign => Token::new(TokenKind::BitwiseOrAssign, value),
        TokenKind::BitwiseXorAssign => Token::new(TokenKind::BitwiseXorAssign, value),
        TokenKind::BitwiseShiftLeftAssign => Token::new(TokenKind::BitwiseShiftLeftAssign, value),
        TokenKind::BitwiseShiftRightAssign => Token::new(TokenKind::BitwiseShiftRightAssign, value),
        TokenKind::BitwiseRotateLeftAssign => Token::new(TokenKind::BitwiseRotateLeftAssign, value),
        TokenKind::BitwiseRotateRightAssign => {
            Token::new(TokenKind::BitwiseRotateRightAssign, value)
        }
        _ => Token::new(TokenKind::Illegal, value),
    }
}

impl Hash for Token {
    fn hash<H: Hasher>(&self, token: &mut H) {
        self.kind.hash(state);
        self.value.hash(state);
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token_creation() {
        let token = Token {
            kind: TokenKind::Identifier("variable".to_string()),
            value: "variable".to_string(),
        };
        assert_eq!(token.kind, TokenKind::Identifier("variable".to_string()));
        assert_eq!(token.value, "variable");
    }

    #[test]
    fn test_issue_token() {
        let ident_token = issue_token(
            TokenKind::Identifier("".to_string()),
            "myVariable".to_string(),
        );
        assert_eq!(
            ident_token.kind,
            TokenKind::Identifier("myVariable".to_string())
        );
        assert_eq!(ident_token.value, "myVariable");

        let int_token = issue_token(TokenKind::Int("u32".to_string()), "42".to_string());
        assert_eq!(int_token.kind, TokenKind::Int("42".to_string()));
        assert_eq!(int_token.value, "42");

        let float_token = issue_token(TokenKind::Float("".to_string()), "3.14".to_string());
        assert_eq!(float_token.kind, TokenKind::Float("3.14".to_string()));
        assert_eq!(float_token.value, "3.14");

        let string_token = issue_token(TokenKind::String("".to_string()), "hello".to_string());
        assert_eq!(string_token.kind, TokenKind::String("hello".to_string()));
        assert_eq!(string_token.value, "hello");

        let true_token = issue_token(TokenKind::True, "true".to_string());
        assert_eq!(true_token.kind, TokenKind::True);
        assert_eq!(true_token.value, "true");

        let false_token = issue_token(TokenKind::False, "false".to_string());
        assert_eq!(false_token.kind, TokenKind::False);
        assert_eq!(false_token.value, "false");

        let assign_token = issue_token(TokenKind::Assign("=".to_string()), "=".to_string());
        assert_eq!(assign_token.kind, TokenKind::Assign("=".to_string()));
        assert_eq!(assign_token.value, "=");

        let plus_token = issue_token(TokenKind::Plus, "+".to_string());
        assert_eq!(plus_token.kind, TokenKind::Plus);
        assert_eq!(plus_token.value, "+");

        let minus_token = issue_token(TokenKind::Minus, "-".to_string());
        assert_eq!(minus_token.kind, TokenKind::Minus);
        assert_eq!(minus_token.value, "-");

        let illegal_token = issue_token(TokenKind::Illegal, "invalid".to_string());
        assert_eq!(illegal_token.kind, TokenKind::Illegal);
        assert_eq!(illegal_token.value, "invalid");
    }

    #[test]
    fn test_keywords() {
        let if_token = issue_token(TokenKind::If, "if".to_string());
        assert_eq!(if_token.kind, TokenKind::If);
        assert_eq!(if_token.value, "if");

        let else_token = issue_token(TokenKind::Else, "else".to_string());
        assert_eq!(else_token.kind, TokenKind::Else);
        assert_eq!(else_token.value, "else");

        let return_token = issue_token(TokenKind::Return, "return".to_string());
        assert_eq!(return_token.kind, TokenKind::Return);
        assert_eq!(return_token.value, "return");

        let while_token = issue_token(TokenKind::While, "while".to_string());
        assert_eq!(while_token.kind, TokenKind::While);
        assert_eq!(while_token.value, "while");

        let for_token = issue_token(TokenKind::For, "for".to_string());
        assert_eq!(for_token.kind, TokenKind::For);
        assert_eq!(for_token.value, "for");

        let in_token = issue_token(TokenKind::In, "in".to_string());
        assert_eq!(in_token.kind, TokenKind::In);
        assert_eq!(in_token.value, "in");

        let break_token = issue_token(TokenKind::Break, "break".to_string());
        assert_eq!(break_token.kind, TokenKind::Break);
        assert_eq!(break_token.value, "break");

        let continue_token = issue_token(TokenKind::Continue, "continue".to_string());
        assert_eq!(continue_token.kind, TokenKind::Continue);
        assert_eq!(continue_token.value, "continue");

        let match_token = issue_token(TokenKind::Match, "match".to_string());
        assert_eq!(match_token.kind, TokenKind::Match);
        assert_eq!(match_token.value, "match");

        let case_token = issue_token(TokenKind::Case, "case".to_string());
        assert_eq!(case_token.kind, TokenKind::Case);
        assert_eq!(case_token.value, "case");

        let default_token = issue_token(TokenKind::Default, "default".to_string());
        assert_eq!(default_token.kind, TokenKind::Default);
        assert_eq!(default_token.value, "default");

        let switch_token = issue_token(TokenKind::Switch, "switch".to_string());
        assert_eq!(switch_token.kind, TokenKind::Switch);
        assert_eq!(switch_token.value, "switch");

        let struct_token = issue_token(TokenKind::Struct, "struct".to_string());
        assert_eq!(struct_token.kind, TokenKind::Struct);
        assert_eq!(struct_token.value, "struct");

        let enum_token = issue_token(TokenKind::Enum, "enum".to_string());
        assert_eq!(enum_token.kind, TokenKind::Enum);
        assert_eq!(enum_token.value, "enum");

        let type_token = issue_token(TokenKind::Type, "type".to_string());
        assert_eq!(type_token.kind, TokenKind::Type);
        assert_eq!(type_token.value, "type");

        let module_token = issue_token(TokenKind::Module, "module".to_string());
        assert_eq!(module_token.kind, TokenKind::Module);
        assert_eq!(module_token.value, "module");

        let import_token = issue_token(TokenKind::Import, "import".to_string());
        assert_eq!(import_token.kind, TokenKind::Import);
        assert_eq!(import_token.value, "import");

        let export_token = issue_token(TokenKind::Export, "export".to_string());
        assert_eq!(export_token.kind, TokenKind::Export);
        assert_eq!(export_token.value, "export");

        let const_token = issue_token(TokenKind::Const, "const".to_string());
        assert_eq!(const_token.kind, TokenKind::Const);
        assert_eq!(const_token.value, "const");

        let static_token = issue_token(TokenKind::Static, "static".to_string());
        assert_eq!(static_token.kind, TokenKind::Static);
        assert_eq!(static_token.value, "static");

        let mut_token = issue_token(TokenKind::Mut, "mut".to_string());
        assert_eq!(mut_token.kind, TokenKind::Mut);
        assert_eq!(mut_token.value, "mut");

        let ref_token = issue_token(TokenKind::Ref, "ref".to_string());
        assert_eq!(ref_token.kind, TokenKind::Ref);
        assert_eq!(ref_token.value, "ref");

        let logical_and_token = issue_token(TokenKind::LogicalAnd, "&&".to_string());
        assert_eq!(logical_and_token.kind, TokenKind::LogicalAnd);
        assert_eq!(logical_and_token.value, "&&");

        let logical_or_token = issue_token(TokenKind::LogicalOr, "||".to_string());
        assert_eq!(logical_or_token.kind, TokenKind::LogicalOr);
        assert_eq!(logical_or_token.value, "||");

        let less_than_equal_token = issue_token(TokenKind::LessThanEqual, "<=".to_string());
        assert_eq!(less_than_equal_token.kind, TokenKind::LessThanEqual);
        assert_eq!(less_than_equal_token.value, "<=");

        let greater_than_equal_token = issue_token(TokenKind::GreaterThanEqual, ">=".to_string());
        assert_eq!(greater_than_equal_token.kind, TokenKind::GreaterThanEqual);
        assert_eq!(greater_than_equal_token.value, ">=");
    }

    #[test]
    fn test_comparison_operators() {
        let equal_token = issue_token(TokenKind::Equal, "==".to_string());
        assert_eq!(equal_token.kind, TokenKind::Equal);
        assert_eq!(equal_token.value, "==");

        let not_equal_token = issue_token(TokenKind::NotEqual, "!=".to_string());
        assert_eq!(not_equal_token.kind, TokenKind::NotEqual);
        assert_eq!(not_equal_token.value, "!=");

        let less_than_token = issue_token(TokenKind::LessThan, "<".to_string());
        assert_eq!(less_than_token.kind, TokenKind::LessThan);
        assert_eq!(less_than_token.value, "<");

        let greater_than_token = issue_token(TokenKind::GreaterThan, ">".to_string());
        assert_eq!(greater_than_token.kind, TokenKind::GreaterThan);
        assert_eq!(greater_than_token.value, ">");
    }

    #[test]
    fn test_bitwise_operators() {
        let shift_left_token = issue_token(TokenKind::BitwiseShiftLeft, "<<".to_string());
        assert_eq!(shift_left_token.kind, TokenKind::BitwiseShiftLeft);
        assert_eq!(shift_left_token.value, "<<");

        let shift_right_token = issue_token(TokenKind::BitwiseShiftRight, ">>".to_string());
        assert_eq!(shift_right_token.kind, TokenKind::BitwiseShiftRight);
        assert_eq!(shift_right_token.value, ">>");

        let rotate_left_token = issue_token(TokenKind::BitwiseRotateLeft, "<<>".to_string());
        assert_eq!(rotate_left_token.kind, TokenKind::BitwiseRotateLeft);
        assert_eq!(rotate_left_token.value, "<<>");
    }
}
