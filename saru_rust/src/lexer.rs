use crate::tokens::TokenKind;


#[derive(Debug)]
pub struct Lexer {
    pub input: Vec<u8>,
    pub position: usize,
    pub read_position: usize,
    pub ch: u8,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let mut lexer = Lexer {
            input: input.as_bytes().to_vec(),
            position: 0,
            read_position: 0,
            ch: 0,
        };
        lexer.read_char();
        lexer
    }
    fn is_letter(ch: u8) -> bool {
        ch.is_ascii_alphabetic() || ch == b'_'
    }
    fn is_digit(ch: u8) -> bool {
        ch.is_ascii_digit()
    }
    pub fn input(&self) -> &str { 
        std::str::from_utf8(&self.input).unwrap()
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }


    pub fn read_position(&self) -> usize {
        self.read_position
    }
    
    pub fn next_token(&mut self) -> TokenKind {
        self.skip_whitespace();
        let token = match self.ch {
            0 => TokenKind::Eof,
        
            b'=' => {
                if self.peek_char(0) == b'=' {
                    self.read_char();
                    TokenKind::Equal
                } else {
                    TokenKind::Assign("=".to_string())
                }
            }

            b'+' => {
                if self.peek_char(0) == b'+' {
                    self.read_char();
                    TokenKind::Increment
                } else {
                    TokenKind::Plus
                }

            }
            b'-' => {
                if self.peek_char(0) == b'-' {
                    self.read_char();
                    TokenKind::Decrement
                } else {
                    TokenKind::Minus
                }
            }
            b'<' => {
                if self.peek_char(0) == b'<' && self.peek_char(self.read_position + 1) == b'>' {
                    self.read_char();
                    self.read_char();
                    TokenKind::BitwiseRotateLeft
                } else if self.peek_char(0) == b'=' {
                    self.read_char();
                    TokenKind::LessThanEqual
                } else if self.peek_char(0) == b'<' {
                    self.read_char();
                    TokenKind::BitwiseShiftLeft
                
                } else {
                    TokenKind::LessThan
                }
            }
            b'>' => {
                if self.peek_char(0) == b'>' && self.peek_char(self.read_position + 1) == b'>' {
                    self.read_char();
                    self.read_char();
                    TokenKind::BitwiseRotateRight
                } else if self.peek_char(0) == b'=' {
                    self.read_char();
                    TokenKind::GreaterThanEqual
                } else {
                    TokenKind::GreaterThan
                }
            }
            
            b'*' => TokenKind::Asterisk,
            b'/' => TokenKind::Divide,
            b'(' => TokenKind::LParen,
            b')' => TokenKind::RParen,
            b'{' => TokenKind::LBrace,
            b'}' => TokenKind::RBrace,
            
            _ => TokenKind::Illegal,
            
        };
        self.read_char();
        token
    }

    fn peek_char(&self, offset: usize) -> u8 {
        let position = self.read_position + offset;
        if position >= self.input.len() {
            0
        } else {
            self.input[position]
        }
    }
    

    pub  fn read_identifier(&mut self) -> String {
        let position = self.position;
        while Lexer::is_letter(self.ch) {
            self.read_char();
        }
       String::from_utf8_lossy(&self.input[position..self.position]).into_owned()
    }
    pub fn read_number(&mut self) -> String {
        let position = self.position;
        while Lexer::is_digit(self.ch) {
            self.read_char();
        }
        String::from_utf8_lossy(&self.input[position..self.position]).into_owned()
    }

    pub fn read_string(&mut self) -> String {
        let position = self.position;
        while self.ch != b'"' {
            self.read_char();
        }
        String::from_utf8_lossy(&self.input[position..self.position]).into_owned()
    }

    fn skip_whitespace(&mut self) {
        while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\n' || self.ch == b'\r' {
            self.read_char();
        }
    }
   
}


#[cfg(test)]
mod tests {
    use super::*;

    
    #[test]
    fn test_read_identifier() {
        let mut lexer = Lexer::new("let five = 5;");
        assert_eq!(lexer.read_identifier(), "let");
    }
    // #[test]
    // fn test_read_number() {
    //     let mut lexer = Lexer::new("let five = 5;");
    //     assert_eq!(lexer.read_number(), "5");
    }
    #[test]
    // fn test_read_string() {
    //     let mut lexer = Lexer::new("let five = 5;");
    //     assert_eq!(lexer.read_string(), "let");
    // }
    #[test]
    fn test_skip_whitespace() {
        let mut lexer = Lexer::new("   let five = 5;");
        lexer.skip_whitespace();
        assert_eq!(lexer.ch, b'l');
    }
    #[test]
    fn test_is_letter() {
        assert_eq!(Lexer::is_letter(b'a'), true);
        assert_eq!(Lexer::is_letter(b'1'), false);
    }
    #[test]
    fn test_is_digit() {
        assert_eq!(Lexer::is_digit(b'1'), true);
        assert_eq!(Lexer::is_digit(b'a'), false);
    }

    // #[test]
    // fn test_next_token() {
    //     let mut lexer = Lexer::new("let five = 5;");
    //     assert_eq!(lexer.next_token(), TokenKind::Let("five".to_string()));
    // }

