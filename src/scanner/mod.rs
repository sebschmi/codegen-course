use crate::error::{Error, Result};

/// The integer type used for literals.
/// Note that since we output C code, this might be truncated further when compiling the C program.
pub type IntType = usize;
/// The real type used for literals.
/// Note that since we output C code, this might be truncated further when compiling the C program.
pub type RealType = f64;

/// The set of tokens output by the scanner.
pub enum Token {
    // Punctuation
    Semicolon,
    Pipe,
    Dot,
    Comma,
    Colon,

    // Parentheses
    OpenParenthesis,
    CloseParenthesis,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,

    // Operators
    AssignOperator,
    EqOperator,
    NeqOperator,
    LtOperator,
    LeqOperator,
    GtOperator,
    GeqOperator,
    OrOperator,    // Special case: alphabetic characters
    AndOperator,   // Special case: alphabetic characters
    PlusOperator,  // Special case: might also be a sign
    MinusOperator, // Special case: might also be a sign
    MulOperator,
    DivOperator,
    ModOperator,

    // Keywords
    Program,
    Procedure,
    Function,
    Begin,
    End,
    Var,
    If,
    Then,
    Else,
    While,
    Do,
    Array,
    Return,

    // Literals
    // we simply forward literals to the C program and do not bother about parsing them here
    // (except for boolean literals, those do not exist in C by default)
    IntegerLiteral(String), // one or more digits
    RealLiteral(String), // two groups of one or more digits, separated by a dot
    StringLiteral(String), // enclosed by '"', where certain characters can be escaped with '\', including '"'

    // Identifiers
    Identifier(String), // starting with a letter, otherwise mixture of letter, digit and underscores
    PredefinedIdentifier(String), // like an identifier, but the special predefined case of Mini-PL
}

/// The type used for input scanning.
/// The scanner type itself is an iterator over tokens, which produces those by reading from an iterator over characters.
///
/// We store a pair of characters, the current character and one as lookahead.
///
/// The scanner also keeps track of the current line and column for better error reporting.
/// The line and column are counted starting from one.
pub struct Scanner<CharacterIterator> {
    /// The iterator over the input characters.
    input: CharacterIterator,
    /// The current character.
    current: Option<char>,
    /// Keep one character as lookahead.
    lookahead: Option<char>,

    /// The line the current character is at.
    line: usize,
    /// The column the current character is at.
    column: usize,
}

impl<CharacterIterator: Iterator<Item = char>> Scanner<CharacterIterator> {
    /// Create and initialise a new scanner over the given input.
    pub fn new(input: CharacterIterator) -> Self {
        let mut result = Self {
            input,
            current: None,
            lookahead: None,
            line: 1,
            column: 1,
        };
        result.advance();
        result.advance();
        result
    }

    /// Advances the position in the input by a single character.
    fn advance(&mut self) {
        if self.current == Some('\n') {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        self.current = self.lookahead;
        self.lookahead = self.input.next();
    }

    /// Advances until the current character is `None` or not a whitespace character.
    fn skip_whitespace(&mut self) {
        while let Some(current) = self.current {
            if current.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Advances until the current character is `None` or not a whitespace character, and additionally not inside a comment.
    fn skip_comments_and_whitespace(&mut self) -> Result<()> {
        loop {
            self.skip_whitespace();
            if let (Some('{'), Some('*')) = (self.current, self.lookahead) {
                // advance twice before first check to not accept {*} as valid comment
                self.advance();

                loop {
                    self.advance();

                    if self.lookahead.is_none() {
                        // we have reached the end of the file without being able to close the comment
                        return Err(Error::UnclosedComment);
                    } else if (Some('*'), Some('}')) == (self.current, self.lookahead) {
                        // advance twice to move completely out of comment
                        self.advance();
                        self.advance();
                        // return to outer loop to check for more whitespace or comments
                        break;
                    }

                    // if we found no end of the comment, we continue searching
                }

                self.skip_whitespace();
            } else {
                return Ok(());
            }
        }
    }

    /// This function parses the numeric literal starting with the current character,
    /// and advances the iterator until current is the last character of the token.
    /// (The iterator is advanced by one after parsing a token successfully, so we cannot advance it to after the token in this function)
    fn parse_integer_or_real_literal(&mut self) -> Result<Token> {
        // it is a programming error to call this function when the current character is not a digit
        assert!(self.current.unwrap().is_digit(10));

        let mut literal = String::new();
        literal.push(self.current.unwrap());

        let mut found_dot = false;
        while let Some(lookahead) = self.lookahead {
            if lookahead == '.' && !found_dot {
                found_dot = true;
                literal.push(lookahead);
            } else if lookahead.is_digit(10) {
                literal.push(lookahead);
            } else {
                break;
            }

            self.advance();
        }

        if found_dot {
            // unwrap cannot fail, as we push at least one character right after construction
            if literal.ends_with('.') {
                // the number part of the literal must start and end with a digit
                return Err(Error::MalformedRealLiteral);
            }

            // check if there is an exponent
            if self.lookahead == Some('e') {
                literal.push('e');
                self.advance();

                // optionally add a sign to the exponent
                if let Some(lookahead) = self.lookahead {
                    if lookahead == '+' || lookahead == '-' {
                        literal.push(lookahead);
                        self.advance();
                    }
                }

                // add the digits of the exponent
                let mut found_digit = false;
                while let Some(lookahead) = self.lookahead {
                    if lookahead.is_digit(10) {
                        found_digit = true;
                        literal.push(lookahead);
                    }

                    self.advance();
                }

                if !found_digit {
                    return Err(Error::MalformedRealLiteral);
                }
            }

            Ok(Token::RealLiteral(literal))
        } else {
            Ok(Token::IntegerLiteral(literal))
        }
    }

    /// This function parses the string literal starting with the current character,
    /// and advances the iterator until current is the last character of the token.
    /// (The iterator is advanced by one after parsing a token successfully, so we cannot advance it to after the token in this function)
    ///
    /// The function must be called when current is '"'.
    fn parse_string_literal(&mut self) -> Result<Token> {
        // it is a programming error to call this function when the current character is not '"'a digit'"'
        assert_eq!(self.current, Some('"'));

        let mut literal = String::new();
        self.advance();

        while let Some(current) = self.current {
            if current == '\\' && self.lookahead == Some('"') {
                literal.push_str("\\\"");
                self.advance();
            } else if current == '"' {
                return Ok(Token::StringLiteral(literal));
            } else {
                literal.push(current);
            }

            self.advance();
        }

        Err(Error::UnclosedStringLiteral)
    }
    /// This function parses the alphanumeric token starting with the current alphabetic character,
    /// and advances the iterator until current is the last character of the token.
    /// (The iterator is advanced by one after parsing a token successfully, so we cannot advance it to after the token in this function)
    ///
    /// This function does not parse numeric literals, so the first character must be alphabetic.
    fn parse_keyword_or_identifier_or_predefined_identifier_token(&mut self) -> Result<Token> {
        // it is a programming error to call this function when the current character is not alphabetic
        assert!(self.current.unwrap().is_ascii_alphabetic());

        let mut literal = String::new();
        literal.push(self.current.unwrap());

        while let Some(lookahead) = self.lookahead {
            if lookahead.is_ascii_alphanumeric() || lookahead == '_' {
                literal.push(lookahead);
                self.advance();
            } else {
                break;
            }
        }

        Ok(match literal.as_str() {
            // Keywords
            "program" => Token::Program,
            "procedure" => Token::Procedure,
            "function" => Token::Function,
            "begin" => Token::Begin,
            "end" => Token::End,
            "var" => Token::Var,
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            "while" => Token::While,
            "do" => Token::Do,
            "array" => Token::Array,
            "return" => Token::Return,
            "or" => Token::OrOperator,
            "and" => Token::AndOperator,

            // Identifiers
            // we transform everything to lower case, since identifiers are case-insensitive
            // Boolean has an uppercase B as specified
            "true" | "false" | "Boolean" | "integer" | "real" | "string" | "read" | "writeln" | "size" => Token::PredefinedIdentifier(literal.to_ascii_lowercase()),
            _ => Token::Identifier(literal.to_ascii_lowercase()),
        })
    }
}

impl<CharacterIterator: Iterator<Item = char>> Iterator for Scanner<CharacterIterator> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Err(error) = self.skip_comments_and_whitespace() {
            return Some(Err(error));
        }

        if let Some(current) = self.current {
            let result = Some(Ok(match current {
                // Punctuation
                ';' => Token::Semicolon,
                '|' => Token::Pipe,
                '.' => Token::Dot,
                ',' => Token::Comma,
                ':' => {
                    if self.lookahead == Some('=') {
                        self.advance();
                        Token::AssignOperator
                    } else {
                        Token::Colon
                    }
                }

                // Parentheses
                '(' => Token::OpenParenthesis,
                ')' => Token::CloseParenthesis,
                '[' => Token::OpenBracket,
                ']' => Token::CloseBracket,
                '{' => Token::OpenBrace,
                '}' => Token::CloseBrace,

                // Operators
                '=' => Token::EqOperator,
                '<' => {
                    if self.lookahead == Some('>') {
                        self.advance();
                        Token::NeqOperator
                    } else if self.lookahead == Some('=') {
                        self.advance();
                        Token::LeqOperator
                    } else {
                        Token::LtOperator
                    }
                }
                '>' => if self.lookahead == Some('=') {
                    self.advance();
                    Token::GeqOperator
                } else {
                    Token::GtOperator
                }
                '+' => Token::PlusOperator,
                '-' => Token::MinusOperator,
                '*' => Token::MulOperator,
                '/' => Token::DivOperator,
                '%' => Token::ModOperator,

                other => if other.is_digit(10) {
                    // integer or real literal
                    match self.parse_integer_or_real_literal() {
                        Ok(token) => token,
                        error => return Some(error),
                    }
                } else if other == '"' {
                    match self.parse_string_literal() {
                        Ok(token) => token,
                        error => return Some(error),
                    }
                } else if other.is_alphabetic() {
                    match self.parse_keyword_or_identifier_or_predefined_identifier_token() {
                        Ok(token) => token,
                        error => return Some(error),
                    }
                } else {
                    return Some(Err(Error::NotTheStartOfAToken));
                },


            }));

            // advance if a token was successfully detected
            self.advance();
            result
        } else {
            // If there are no characters left, we are done.
            None
        }
    }
}