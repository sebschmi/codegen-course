use crate::error::{scanner_error, Result, ScannerErrorKind};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};

/// The set of tokens output by the scanner.
#[derive(Debug, Clone, Eq, PartialEq)]
#[allow(missing_docs)]
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
    NotOperator, // Special case: alphabetic characters

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
    Of,
    Return,
    Assert,

    // Literals
    // we simply forward literals to the C program and do not bother about parsing them here
    // (except for boolean literals, those do not exist in C by default)
    IntegerLiteral(String), // one or more digits
    RealLiteral(String),    // two groups of one or more digits, separated by a dot
    StringLiteral(String), // enclosed by '"', where certain characters can be escaped with '\', including '"'

    // Identifiers
    Identifier {
        original: String,
        lower_case: String,
    }, // starting with a letter, otherwise mixture of letter, digit and underscores
    PredefinedIdentifier {
        original: String,
        lower_case: String,
    }, // like an identifier, but the special predefined case of Mini-PL
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

impl<CharacterIterator: Iterator<Item = Result<char>>> Scanner<CharacterIterator> {
    /// Create and initialise a new scanner over the given input.
    /// The scanner is initialised with the first two characters of the given input.
    pub fn new(input: CharacterIterator) -> Result<Self> {
        let mut result = Self {
            input,
            current: None,
            lookahead: None,

            line: 1,
            column: 1,
        };
        result.advance()?;
        result.advance()?;
        result.line = 1;
        result.column = 1;
        Ok(result)
    }

    /// Advances the position in the input by a single character.
    fn advance(&mut self) -> Result<ScanInterval> {
        if self.current == Some('\n') {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        self.current = self.lookahead;
        self.lookahead = match self.input.next() {
            None => None,
            Some(Ok(character)) => Some(character),
            Some(Err(error)) => return Err(error),
        };
        Ok(self.current_interval())
    }

    /// Advances until the current character is `None` or not a whitespace character.
    fn skip_whitespace(&mut self) -> Result<ScanInterval> {
        let mut interval = self.current_interval();
        while let Some(current) = self.current {
            if current.is_whitespace() {
                interval = interval.extend_clone(&self.current_interval());
                self.advance()?;
            } else {
                break;
            }
        }
        Ok(interval)
    }

    /// Advances until the current character is `None` or not a whitespace character, and additionally not inside a comment.
    fn skip_comments_and_whitespace(&mut self) -> Result<ScanInterval> {
        let mut interval = self.current_interval();
        loop {
            // skip whitespace between comments
            interval = interval.extend_clone(&self.skip_whitespace()?);

            if let (Some('{'), Some('*')) = (self.current, self.lookahead) {
                // advance twice before first check to not accept {*} as valid comment
                self.advance()?;

                loop {
                    self.advance()?;

                    if self.lookahead.is_none() {
                        // we have reached the end of the file without being able to close the comment
                        return Err(scanner_error(
                            ScanInterval::single(self.line, self.column),
                            ScannerErrorKind::UnclosedComment,
                        ));
                    } else if (Some('*'), Some('}')) == (self.current, self.lookahead) {
                        // advance twice to move completely out of comment
                        self.advance()?;
                        self.advance()?;
                        // return to outer loop to check for more whitespace or comments
                        break;
                    }

                    // if we found no end of the comment, we continue searching
                }
            } else {
                return Ok(interval);
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

            self.advance()?;
        }

        if found_dot {
            // unwrap cannot fail, as we push at least one character right after construction
            if literal.ends_with('.') {
                // the number part of the literal must start and end with a digit
                return Err(scanner_error(
                    ScanInterval::single(self.line, self.column),
                    ScannerErrorKind::MalformedRealLiteral,
                ));
            }

            // check if there is an exponent
            if self.lookahead == Some('e') {
                literal.push('e');
                self.advance()?;

                // optionally add a sign to the exponent
                if let Some(lookahead) = self.lookahead {
                    if lookahead == '+' || lookahead == '-' {
                        literal.push(lookahead);
                        self.advance()?;
                    }
                }

                // add the digits of the exponent
                let mut found_digit = false;
                while let Some(lookahead) = self.lookahead {
                    if lookahead.is_digit(10) {
                        found_digit = true;
                        literal.push(lookahead);
                        self.advance()?;
                    } else {
                        break;
                    }
                }

                if !found_digit {
                    return Err(scanner_error(
                        ScanInterval::single(self.line, self.column),
                        ScannerErrorKind::MalformedRealLiteral,
                    ));
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
        self.advance()?;

        while let Some(current) = self.current {
            if current == '\\' && self.lookahead == Some('"') {
                literal.push_str("\\\"");
                self.advance()?;
            } else if current == '"' {
                return Ok(Token::StringLiteral(literal));
            } else {
                literal.push(current);
            }

            self.advance()?;
        }

        Err(scanner_error(
            ScanInterval::single(self.line, self.column),
            ScannerErrorKind::UnclosedStringLiteral,
        ))
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
                self.advance()?;
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
            "of" => Token::Of,
            "return" => Token::Return,
            "or" => Token::OrOperator,
            "and" => Token::AndOperator,
            "not" => Token::NotOperator,
            "assert" => Token::Assert,

            // Identifiers
            literal => {
                // we transform everything to lower case, since identifiers and predefined identifiers are case-insensitive
                let lower_case = literal.to_ascii_lowercase();
                match lower_case.as_str() {
                    "true" | "false" | "boolean" | "integer" | "real" | "string" | "read"
                    | "writeln" | "size" => Token::PredefinedIdentifier {
                        lower_case,
                        original: literal.to_owned(),
                    },
                    _ => Token::Identifier {
                        lower_case: literal.to_ascii_lowercase(),
                        original: literal.to_owned(),
                    },
                }
            }
        })
    }

    /// Returns the current character as [ScanInterval].
    /// This is the first character after the last returned token, or the first character of the input directly after construction.
    pub fn current_interval(&self) -> ScanInterval {
        ScanInterval::single(self.line, self.column)
    }
}

impl<CharacterIterator: Iterator<Item = Result<char>>> Iterator for Scanner<CharacterIterator> {
    type Item = (Result<Token>, ScanInterval);

    fn next(&mut self) -> Option<Self::Item> {
        if let Err(error) = self.skip_comments_and_whitespace() {
            return Some((Err(error), self.current_interval()));
        }

        if let Some(current) = self.current {
            let interval = self.current_interval();

            let token = match current {
                // Punctuation
                ';' => Token::Semicolon,
                '|' => Token::Pipe,
                '.' => Token::Dot,
                ',' => Token::Comma,
                ':' => {
                    if self.lookahead == Some('=') {
                        if let Err(error) = self.advance() {
                            return Some((Err(error), interval));
                        }
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
                        if let Err(error) = self.advance() {
                            return Some((Err(error), interval));
                        }
                        Token::NeqOperator
                    } else if self.lookahead == Some('=') {
                        if let Err(error) = self.advance() {
                            return Some((Err(error), interval));
                        }
                        Token::LeqOperator
                    } else {
                        Token::LtOperator
                    }
                }
                '>' => {
                    if self.lookahead == Some('=') {
                        if let Err(error) = self.advance() {
                            return Some((Err(error), interval));
                        }
                        Token::GeqOperator
                    } else {
                        Token::GtOperator
                    }
                }
                '+' => Token::PlusOperator,
                '-' => Token::MinusOperator,
                '*' => Token::MulOperator,
                '/' => Token::DivOperator,
                '%' => Token::ModOperator,

                other => {
                    if other.is_digit(10) {
                        // integer or real literal
                        match self.parse_integer_or_real_literal() {
                            Ok(token) => token,
                            Err(error) => return Some((Err(error), interval)),
                        }
                    } else if other == '"' {
                        match self.parse_string_literal() {
                            Ok(token) => token,
                            Err(error) => return Some((Err(error), interval)),
                        }
                    } else if other.is_alphabetic() {
                        match self.parse_keyword_or_identifier_or_predefined_identifier_token() {
                            Ok(token) => token,
                            Err(error) => return Some((Err(error), interval)),
                        }
                    } else {
                        return Some((
                            Err(scanner_error(
                                ScanInterval::single(self.line, self.column),
                                ScannerErrorKind::NotTheStartOfAToken,
                            )),
                            interval,
                        ));
                    }
                }
            };

            let interval = interval.extend_clone(&self.current_interval());
            // advance if a token was successfully detected
            if let Err(error) = self.advance() {
                return Some((Err(error), self.current_interval()));
            }
            Some((Ok(token), interval))
        } else {
            // If there are no characters left, we are done.
            None
        }
    }
}

/// The interval a scanned token takes up in the source file.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ScanInterval {
    /// The line this node starts in.
    pub start_line: usize,
    /// The line this node ends in, inclusive.
    pub end_line: usize,
    /// The column this node starts in.
    pub start_column: usize,
    /// The column this node ends in, exclusive.
    pub end_column: usize,
}

impl ScanInterval {
    /// A single character scan interval.
    pub fn single(line: usize, column: usize) -> Self {
        Self {
            start_line: line,
            end_line: line,
            start_column: column,
            end_column: column + 1,
        }
    }

    /// Extend the interval with the given interval, returning a new instance.
    /// The result will have the minimum start position and maximum end position of the two intervals, even if they do not overlap.
    pub fn extend_clone(&self, other: &ScanInterval) -> Self {
        let mut result = self.clone();
        match result.start_line.cmp(&other.start_line) {
            Ordering::Less => {}
            Ordering::Equal => {
                result.start_column = result.start_column.min(other.start_column);
            }
            Ordering::Greater => {
                result.start_line = other.start_line;
                result.start_column = other.start_column;
            }
        }
        match result.end_line.cmp(&other.end_line) {
            Ordering::Less => {
                result.end_line = other.end_line;
                result.end_column = other.end_column;
            }
            Ordering::Equal => {
                result.end_column = result.end_column.max(other.end_column);
            }
            Ordering::Greater => {}
        }
        result
    }
}

impl Display for ScanInterval {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.start_line < self.end_line {
            write!(f, "{}-{}:", self.start_line, self.end_line)?;
        } else {
            write!(f, "{}:", self.start_line)?;
        }
        if self.start_column < self.end_column {
            write!(f, "{}-{}", self.start_column, self.end_column)
        } else {
            write!(f, "{}", self.start_column)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::scanner::Scanner;
    use crate::scanner::Token;

    #[test]
    fn test_large_program() {
        let program = "program na_5_Rr4_; functionand and function Boolean boolean true False false and procedure ( ) )()(%54/  | }]{[}\n\n\tab.size+size+.-*/-4+4(+4)5.5,643-5.4e+3 \" \\\" \\\\ £$€@\"(abc)0.1e-3+  \n\t";
        let scanner = Scanner::new(program.chars().map(Ok)).unwrap();
        let tokens: Vec<_> = scanner.map(|token| token.0.unwrap()).collect();
        use Token::*;
        assert_eq!(
            tokens,
            vec![
                Program,
                Identifier {
                    lower_case: "na_5_rr4_".to_string(),
                    original: "na_5_Rr4_".to_string()
                },
                Semicolon,
                Identifier {
                    lower_case: "functionand".to_string(),
                    original: "functionand".to_string()
                },
                AndOperator,
                Function,
                PredefinedIdentifier {
                    lower_case: "boolean".to_string(),
                    original: "Boolean".to_string()
                },
                PredefinedIdentifier {
                    lower_case: "boolean".to_string(),
                    original: "boolean".to_string()
                },
                PredefinedIdentifier {
                    lower_case: "true".to_string(),
                    original: "true".to_string()
                },
                PredefinedIdentifier {
                    lower_case: "false".to_string(),
                    original: "False".to_string()
                },
                PredefinedIdentifier {
                    lower_case: "false".to_string(),
                    original: "false".to_string()
                },
                AndOperator,
                Procedure,
                OpenParenthesis,
                CloseParenthesis,
                CloseParenthesis,
                OpenParenthesis,
                CloseParenthesis,
                OpenParenthesis,
                ModOperator,
                IntegerLiteral("54".to_string()),
                DivOperator,
                Pipe,
                CloseBrace,
                CloseBracket,
                OpenBrace,
                OpenBracket,
                CloseBrace,
                Identifier {
                    lower_case: "ab".to_string(),
                    original: "ab".to_string()
                },
                Dot,
                PredefinedIdentifier {
                    lower_case: "size".to_string(),
                    original: "size".to_string()
                },
                PlusOperator,
                PredefinedIdentifier {
                    lower_case: "size".to_string(),
                    original: "size".to_string()
                },
                PlusOperator,
                Dot,
                MinusOperator,
                MulOperator,
                DivOperator,
                MinusOperator,
                IntegerLiteral("4".to_string()),
                PlusOperator,
                IntegerLiteral("4".to_string()),
                OpenParenthesis,
                PlusOperator,
                IntegerLiteral("4".to_string()),
                CloseParenthesis,
                RealLiteral("5.5".to_string()),
                Comma,
                IntegerLiteral("643".to_string()),
                MinusOperator,
                RealLiteral("5.4e+3".to_string()),
                StringLiteral(" \\\" \\\\ £$€@".to_string()),
                OpenParenthesis,
                Identifier {
                    lower_case: "abc".to_string(),
                    original: "abc".to_string()
                },
                CloseParenthesis,
                RealLiteral("0.1e-3".to_string()),
                PlusOperator,
            ]
        );
    }
}
