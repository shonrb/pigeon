//
// lex.rs
// Converts code from text to a token stream
//
use std::str::FromStr;
use std::fmt::Debug;
use regex::{Regex, Match};

use crate::bracketstack::BracketStack;

// The regex pattern for splitting tokens
const PATTERN: &str = concat!(
    r#"\s*\n|"#,                                      // Newlines
    r#"\s*([[:alpha:]_][\w]*)|"#,                     // Words
    r#"\s*(\d[\w\.]*)|"#,                             // Number literals
    r#"\s*("[^"\\]*(?:\\.[^"\\]*)*("|\n))|"#,         // String literals
    r#"\s*('[^'\\]*(?:\\.[^'\\]*)*('|\n))|"#,         // Char literals
    r#"\s*([!$%&()*+,\\\-\./:;<=>?@\[\]^_`{|}~]+)|"#, // Operators
    r#"\s*(#.*)|"#,                                   // Comments
    r#"\s*(.*)"#                                      // Everything else
);

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Error(String),
    EndLine,               
    NewLine,               
    Nothing,
    Identifier(String),
    IntLiteral(i64),       
    FloatLiteral(f64),
    StringLiteral(String), 
    CharLiteral(char),
    True,
    False,
    TypeInt,
    TypeFloat,
    TypeString,
    TypeChar,
    TypeBool,
    TypeNothing,
    TypeList,
    TypeFunc,
    TypeType,
    If,
    Elif,                  
    Else,
    While,       
    For,
    In,
    To,
    Return,              
    Yield,
    Break,
    Continue,
    Arrow,                 
    BracketOpen,
    BracketClose,          
    SquareBracketOpen,
    SquareBracketClose,    
    CurlyBracketOpen,
    CurlyBracketClose,     
    Comma,
    Add,
    Subtract,              
    Multiply,
    Divide,                
    Mod,
    BWOr,                  
    BWAnd,                 
    BWXor,                 
    BWNot,
    BWShiftLeft,           
    BWShiftRight,
    Not,                   
    And,                   
    Or,                    
    Equality,              
    Inequality,            
    LessThan,              
    GreaterThan,           
    LessThanOrEqual,       
    GreaterThanOrEqual,    
    Assignment,            
    AddAssign,             
    SubAssign,
    MulAssign,             
    DivAssign,
    ModAssign,             
    BWAndAssign,
    BWXorAssign,           
    BWOrAssign,
    BWShiftLeftAssign,     
    BWShiftRightAssign
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenType,
    pub line: usize,
    pub start: usize,
    pub end: usize
}

struct Tokeniser {
    tokens: Vec<Token>,
    line: usize,
    current_token_start: usize,
    current_token_end: usize,
    line_offset: usize
}

// Main tokenising function
pub fn tokenise(text: &str) -> Vec<Token> {
    let matches: Vec<Match> = Regex::new(PATTERN)
        .unwrap()
        .find_iter(text)
        .collect();
        
    let mut state = Tokeniser::new();
    for tok in matches {
        state.extract_token(&tok);
    }
    state.finalise();
    state.results()
}

impl Tokeniser {
    pub fn new() -> Self { Self {
        tokens:              Vec::new(),
        line:                1,
        current_token_start: 0,
        current_token_end:   0,
        line_offset:         0
    }}

    
    // Extract a token from a regex match
    pub fn extract_token(&mut self, tok_match: &Match) {
        let pred = |x: char| x.is_ascii_whitespace() && x != '\n';
        let string = tok_match
            .as_str()
            .trim_matches(pred);

        // Get the location in the text
        self.current_token_start = tok_match.start();
        self.current_token_end   = tok_match.end();

        if string.len() == 0 {
            return;
        }

        let first = string.chars()
            .nth(0)
            .unwrap();        

        match first {
            '\n' => {
                self.add_token(TokenType::NewLine);
                self.line += string.matches("\n").count();
                self.line_offset = self.current_token_end;
            },
            '"' | '\'' 
                => self.extract_string_or_char_lit(string, first),
            '#' 
                => {},
            c if c.is_ascii_alphabetic() || c == '_' 
                => self.extract_word(string),
            c if c.is_ascii_digit()
                => self.extract_number(string),
            c if c.is_ascii_punctuation() 
                => self.extract_symbols(string),
            _ 
                => self.add_error("Invalid character")
    
        }
    }

    fn add_token(&mut self, token: TokenType) {
        self.tokens.push(Token {
            kind:  token,
            line:  self.line,
            start: self.current_token_start - self.line_offset,
            end:   self.current_token_end   - self.line_offset // Extra newline
        });
    }

    fn add_error(&mut self, msg: &str) {
        self.add_token( 
            TokenType::Error(msg.to_string()) 
        );
    }

    fn parse_number<T: FromStr>(&mut self, s: &str, make: fn(T) -> TokenType) {
        // Add a token from the result of make() called 
        // with the result of a string parsed to type T
        let parsed = s.parse::<T>();
        match parsed {
            Ok(res) => self.add_token(make(res)),
            Err(_)  => self.add_error("Invalid token")
        }
    }

    fn extract_number(&mut self, string: &str) {
        if string.contains(".") {
            self.parse_number::<f64>(string, |x| TokenType::FloatLiteral(x));
        } else {
            self.parse_number::<i64>(string, |x| TokenType::IntLiteral(x));
        }
    }

    fn extract_word(&mut self, string: &str) {
        self.add_token(match string {
            "if"       => TokenType::If,         "elif"        => TokenType::Elif,
            "else"     => TokenType::Else,       "while"       => TokenType::While,
            "return"   => TokenType::Return,     "true"        => TokenType::True,   
            "false"    => TokenType::False,      "and"         => TokenType::And,    
            "or"       => TokenType::Or,         "not"         => TokenType::Not,    
            "nothing"  => TokenType::Nothing,    "yield"       => TokenType::Yield, 
            "int"      => TokenType::TypeInt,    "float"       => TokenType::TypeFloat,
            "string"   => TokenType::TypeString, "char"        => TokenType::TypeChar,
            "bool"     => TokenType::TypeBool,   "nothingtype" => TokenType::TypeNothing,
            "list"     => TokenType::TypeList,   "type"        => TokenType::TypeType,
            "function" => TokenType::TypeFunc,   "break"       => TokenType::Break,
            "continue" => TokenType::Continue,   "for"         => TokenType::For,
            "in"       => TokenType::In,         "to"          => TokenType::To,
            // If no match is found, treat the word as an identifier
            _         => TokenType::Identifier(string.to_string())
        });
    }

    fn extract_string_or_char_lit(&mut self, string: &str, delim: char) {
        let size = string.len();
        // Check if the token is unterminated
        if size == 1 || !string.ends_with(delim) {
            self.add_error("Unterminated literal");
            // If the token includes a newline, correct the line number
            if string.ends_with('\n') {
                self.line += 1;
                self.add_token(TokenType::NewLine);
            }
            return;
        }
        // Remove the delimiters
        let mut result = String::from(&string[1..size-1]);

        // Replace escape sequences with their actual values
        let escape_sequences = [
            ("\\\\", '\\'),
            ("\\n",  '\n'),
            ("\\r",  '\r'),
            ("\\t",  '\t'),
            (&format!("\\{}", delim), delim)
        ];
        for (substring, replace) in escape_sequences {
            result = result.replace(substring, &String::from(replace));
        }
        
        // Char literals should be one character long
        if delim == '\'' && result.len() != 1 {
            self.add_error("Invalid character literal"); 
            return; 
        } 
        
        self.add_token(match delim {
            '\'' => TokenType::CharLiteral(result.chars().nth(0).unwrap()),
            '"'  => TokenType::StringLiteral(result.to_string()),
            _    => panic!("This should never happen")
        });
    }

    fn extract_symbols(&mut self, string: &str) {
        // Extract symbol tokens from a string of symbols, where
        // there could be multiple chained together 
        // as well as multi char operators
        // For example: "+-!=/" will parse to +, -, !=, /
        let block_start = self.current_token_start;
        let block_end   = self.current_token_end;
        let     len                      = string.len();
        let mut current_begin            = 0;
        let mut current_end              = 1;
        let mut token_end                = len;
        let mut token: Option<TokenType> = None;

        while current_begin < len {
            let slice = &string[current_begin .. current_end];
            let res = Self::get_symbol_token(slice);

            // The longest token takes priority, so replace 
            // the current token if a new one is found
            if res.is_some() {
                token = res;
                token_end = current_end;
            }

            // If we reached the end, push the longest found token
            // and continue searching after it. If none were found
            // then we've encountered an invalid operator
            if current_end == len {
                self.current_token_start = block_start + current_begin;
                self.current_token_end   = block_end   + current_end;
                match token {
                    None    => self.add_error("Invalid token"),
                    Some(t) => self.add_token(t)
                }
                current_begin = token_end;
                current_end   = token_end;
                token = None;
            }
            current_end += 1;
        }
    }

    fn get_symbol_token(string: &str) -> Option<TokenType> {
        Some(match string {
            "("   => TokenType::BracketOpen,        ")"   => TokenType::BracketClose,
            "["   => TokenType::SquareBracketOpen,  "]"   => TokenType::SquareBracketClose,
            "{"   => TokenType::CurlyBracketOpen,   "}"   => TokenType::CurlyBracketClose,
            ","   => TokenType::Comma,              "="   => TokenType::Assignment,
            "+"   => TokenType::Add,                "-"   => TokenType::Subtract,
            "*"   => TokenType::Multiply,           "/"   => TokenType::Divide,
            "|"   => TokenType::BWOr,               "&"   => TokenType::BWAnd,
            "^"   => TokenType::BWXor,              "!"   => TokenType::BWNot,
            ">"   => TokenType::GreaterThan,        "<"   => TokenType::LessThan,
            "=="  => TokenType::Equality,           "!="  => TokenType::Inequality,
            ">="  => TokenType::GreaterThanOrEqual, "<="  => TokenType::LessThanOrEqual,
            ";"   => TokenType::EndLine,            "%"   => TokenType::Mod,
            "<<"  => TokenType::BWShiftLeft,        ">>"  => TokenType::BWShiftRight,
            "+="  => TokenType::AddAssign,          "-="  => TokenType::SubAssign,
            "*="  => TokenType::MulAssign,          "/="  => TokenType::DivAssign,
            "%="  => TokenType::ModAssign,          "&="  => TokenType::BWAndAssign,
            "^="  => TokenType::BWXorAssign,        "|="  => TokenType::BWOrAssign,
            "<<=" => TokenType::BWShiftLeftAssign,  ">>=" => TokenType::BWShiftRightAssign,
            "->"  => TokenType::Arrow, 
            _    => return None
        })
    }

    pub fn finalise(&mut self) {
        // Replace endlines with newlines, where a line is ended
        let mut prev               = &TokenType::NewLine;
        let mut bracket_stack      = BracketStack::new();

        for i in 0..self.tokens.len() {
            let token         = &self.tokens[i];
            let kind          = &token.kind;
            bracket_stack.evaluate_token(kind);

            // Endlines should replace newlines at the end of a complete 
            // expression, so not inside brackets, at the beginning of
            // new scopes, or after empty lines
            if kind == &TokenType::NewLine
            && prev != &TokenType::NewLine
            && prev != &TokenType::CurlyBracketOpen
            && bracket_stack.top_is_scoped()
            && !self.token_is_connected(prev, i+1) {
                // Find out if the previous token is connected to an upcoming 
                // one, in which case there should be no endlines between them
                self.tokens[i].kind = TokenType::EndLine;
                prev = &TokenType::NewLine;
            } else {
                prev = kind;
            }
        }

        // Remove all remaining newlines
        for i in (0..self.tokens.len()).rev() {
            if &self.tokens[i].kind == &TokenType::NewLine {
                self.tokens.remove(i);
            }
        }
    }
    
    // Some tokens are connected to others, meaning 
    // there should be no endlines between them.
    fn token_is_connected(&self, start: &TokenType, search_from: usize) -> bool {
        const CONNECTIONS: [(TokenType, TokenType); 3] = [
            (TokenType::CurlyBracketClose, TokenType::Elif),
            (TokenType::CurlyBracketClose, TokenType::Else),
            (TokenType::Arrow,             TokenType::CurlyBracketOpen)
        ];

        for i in search_from..self.tokens.len() {
            let end = &self.tokens[i].kind;
            if end != &TokenType::NewLine {
                // Non newline found, test if any of the connections apply
                for (a, b) in CONNECTIONS {
                    if start == &a && end == &b {
                        return true;
                    }
                }
                break;
            }
        }

        false
    }

    pub fn results(self) -> Vec<Token> {
        self.tokens
    }
}