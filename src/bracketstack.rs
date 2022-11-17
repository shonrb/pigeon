//
// bracketstack.rs
// Helper structure to find matching tokens
//
use crate::lex::TokenType;

pub struct BracketStack {
    stack: Vec<TokenType>
}

impl BracketStack {
    pub fn new() -> Self { Self {
        stack: Vec::new()
    }}

    pub fn is_empty(&self) -> bool {
        self.stack.len() == 0
    }

    pub fn top_is_scoped(&self) -> bool {
        self.is_empty() 
        || self.stack.last().unwrap() == &TokenType::CurlyBracketOpen
    }

    pub fn evaluate_token(&mut self, token: &TokenType) {
        // If the token closes the current bracket, 
        // remove the top of the bracket stack
        if !self.is_empty() {
            let stack_top = self.stack.last().unwrap();
            let closing   = Self::get_matching_token(&stack_top).unwrap();
            if token == &closing {
                self.stack.pop();
            }
        }
        
        // If the current token opens a new bracket,
        // add it to the bracket stack
        let opening = Self::get_matching_token(token);
        if opening.is_some() {
            self.stack.push(token.clone());
        }
    }

    pub fn get_matching_token(token: &TokenType) -> Option<TokenType> {
        Some(match token {
            TokenType::BracketOpen       => TokenType::BracketClose,
            TokenType::CurlyBracketOpen  => TokenType::CurlyBracketClose,
            TokenType::SquareBracketOpen => TokenType::SquareBracketClose,
            TokenType::If | TokenType::Elif | TokenType::While | TokenType::For
                => TokenType::CurlyBracketOpen,
            _ => return None
        })
    }
}