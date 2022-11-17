//
// parse.rs
// Converts a token stream to an abstract syntax tree
//
use std::rc::Rc;
use crate::lex::{Token, TokenType}; 
use crate::object::*;
use crate::bracketstack::BracketStack;

pub type AST = Vec<Expression>;

#[derive(Debug)]
pub struct Conditional {
    pub condition: Expression,
    pub scope: AST
}

#[derive(Debug)]
pub enum ExpressionType {
    Literal(Box<dyn Object>),
    Identifier(String),
    BinaryOp { 
        lhs: Expression, 
        op: TokenType, 
        rhs: Expression 
    },
    UnaryOp {
        op: TokenType,
        v: Expression
    },
    RawScope(AST),
    If { 
        if_: Conditional, 
        elif: Vec<Conditional>, 
        else_: Option<AST>
    },
    While(Conditional),
    For {
        identifier: String, 
        indexable: Expression,
        body: AST
    },
    ListConstruct(Expression),
    Index { 
        operand: Expression, 
        index: Expression 
    },
    Function {
        params: Vec<String>,
        body: AST,
    },
    FunctionCall {
        function: Expression,
        args: Option<Expression>
    },
    Return(Expression),
    Yield(Expression),
    Break,
    Continue,
    Error(String)
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub start_line:  usize,
    pub end_line:    usize,
    pub start_index: usize,
    pub end_index:   usize,
    pub kind:        Rc<ExpressionType>
}

struct ParseSlice<'a> {
    tokens: &'a Vec<Token>,
    start: usize,
    end: usize
}

pub fn parse(tokens: &Vec<Token>) -> AST {
    let slice = ParseSlice::new(tokens);
    slice.parse_multi_expr()
}

fn get_operator_precedence(token: &TokenType) -> Option<usize> {
    Some(match token {
        TokenType::Multiply | TokenType::Divide | TokenType::Mod 
            => 2,
        TokenType::Add | TokenType::Subtract 
            => 3,
        TokenType::BWShiftLeft | TokenType::BWShiftRight 
            => 4,
        TokenType::GreaterThan     | TokenType::LessThan | 
        TokenType::LessThanOrEqual | TokenType::GreaterThanOrEqual
            => 5,
        TokenType::Equality | TokenType::Inequality
            => 6,
        TokenType::BWAnd
            => 7,
        TokenType::BWXor
            => 8,
        TokenType::BWOr
            => 9,
        TokenType::And
            => 10,
        TokenType::Or
            => 11,
        TokenType::To
            => 12,
        TokenType::Assignment  | TokenType::AddAssign  | TokenType::SubAssign   | 
        TokenType::MulAssign   | TokenType::DivAssign  | TokenType::ModAssign   |
        TokenType::BWAndAssign | TokenType::BWOrAssign | TokenType::BWXorAssign |
        TokenType::BWShiftLeftAssign | TokenType::BWShiftRightAssign
            => 13,
        TokenType::Comma
            => 14,
        _ => return None
    })
}

fn reduce_assignment_operator(token: &TokenType) -> Option<TokenType> {
    // Reduces an assignment syntactic sugar 
    // operator into its base operator (+= becomes +)
    Some(match token {
        TokenType::AddAssign          => TokenType::Add, 
        TokenType::SubAssign          => TokenType::Subtract, 
        TokenType::MulAssign          => TokenType::Multiply, 
        TokenType::DivAssign          => TokenType::Divide, 
        TokenType::ModAssign          => TokenType::Mod, 
        TokenType::BWAndAssign        => TokenType::BWAnd,
        TokenType::BWOrAssign         => TokenType::BWOr,
        TokenType::BWXorAssign        => TokenType::BWXor,
        TokenType::BWShiftLeftAssign  => TokenType::BWShiftLeftAssign,
        TokenType::BWShiftRightAssign => TokenType::BWShiftRightAssign,
        _ => return None
    })
}

fn get_unary_operator_precedence(token: &TokenType) -> Option<usize> {
    Some(match token {
        TokenType::Add    | TokenType::Subtract => 1,
        TokenType::Return | TokenType::Yield    => 13,
        _ => return None
    })
}

fn operator_associates_left(prec_level: usize) -> bool {
    prec_level != 13
}

impl Expression {
    pub fn kind(&self) -> &ExpressionType {  
        &self.kind
    }
}

impl<'a> ParseSlice<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self { Self {
        tokens: tokens,
        start:  0,
        end:    tokens.len()
    }}

    fn subslice(&self, new_start: usize, new_end: usize) -> Self { Self {
        tokens: self.tokens,
        start:  new_start,
        end:    new_end
    }}

    fn get(&self, i: usize) -> &TokenType {
        &self.tokens[i].kind
    }

    fn first(&self) -> &TokenType {
        self.get(self.start)
    }

    fn last(&self) -> &TokenType {
        self.get(self.end-1)
    }

    fn len(&self) -> usize {
        self.end - self.start
    }

    fn make_expr(&self, kind: ExpressionType) -> Expression {
        let a = &self.tokens[self.start.min(self.end-1)];
        let b = &self.tokens[self.end-1];
        Expression {
            start_line:  a.line,
            end_line:    b.line,
            start_index: a.start,
            end_index:   b.end,
            kind:        Rc::new(kind)
        }
    }

    fn parsing_error(&self, msg: &str) -> Expression {
        self.make_expr(ExpressionType::Error(msg.to_string()))
    }

    pub fn parse_multi_expr(&self) -> AST {
        let mut exprs: AST = Vec::new();
        let mut bracket_stack    = BracketStack::new();
        let mut segment_start    = self.start;
        
        for mut i in self.start..self.end {
            let token = self.get(i);
            bracket_stack.evaluate_token(token);
            
            // Check if the expression has ended, meaning we 
            // aren't in a bracket, the current token is a newline 
            // or a EOF, and the next significant token is not an else
            let last        = i == self.end-1;
            let is_line_end = token == &TokenType::EndLine;

            if bracket_stack.is_empty() && (is_line_end || last) {
                if last && !is_line_end { 
                    i = self.end; 
                }
                if i > segment_start {
                    let expr = self
                        .subslice(segment_start, i)
                        .parse_segment();
                    exprs.push(expr);
                }
                segment_start = i + 1;
            }
        }

        exprs
    }

    fn parse_segment(&self) -> Expression {
        if self.len() == 0 {
            return self.parsing_error("Expected token");
        }

        // Search for operators. The operators with lowest precedence
        // should be placed at the shallowest point in the tree so prioritise
        // them while searching
        let mut lowest_prec_operator: Option<(TokenType, usize)> = None;
        let mut lowest_prec_level                                = 0;
        let mut bracket_stack                                    = BracketStack::new();
        let mut prev_was_operator                                = true;
        let mut unary_op_precedence: Option<usize>               = None;

        for i in self.start..self.end {
            let token = self.get(i);
            if token == &TokenType::NewLine {
                continue;
            }

            // If the previous token was an operator and this one is a + or -,
            // it's a prefix to an operand and should be ignored
            let unary_op_possible = prev_was_operator;
            prev_was_operator = false;
            let unary_op = get_unary_operator_precedence(token);
            if unary_op.is_some() && unary_op_possible {
                unary_op_precedence = unary_op;
                continue;
            }
            
            // Check how the token affects the bracket we're in, if we're currently
            // in one then we should ignore any operators found
            bracket_stack.evaluate_token(token);
            if !bracket_stack.is_empty() {
                continue;
            }  
            
            // Check the token's operator precedence
            let prec = get_operator_precedence(token);
            
            match prec {
                Some(plevel) => {
                    // If we've encountered a unary operator, we shouldn't consider
                    // this unless the precedence is lower
                    if unary_op_precedence.is_some() 
                    && unary_op_precedence.unwrap() > plevel {
                        continue;
                    }
                    unary_op_precedence = None;
                    
                    // Otherwise
                    prev_was_operator = true;

                    if plevel > lowest_prec_level {
                        // Token is an operator and has lower precedence 
                        // than the current lowest one, replace it.
                        lowest_prec_level = plevel;
                        lowest_prec_operator = Some((token.clone(), i));
                        
                    } else if plevel == lowest_prec_level 
                    &&        operator_associates_left(plevel) {
                        // Token has the same precedence, if this prec
                        // level is right to left associative then replace it
                        lowest_prec_operator = Some((token.clone(), i));
                    }
                }
                None => {}, // Token is not an operator, ignore
            }
        }

        if lowest_prec_operator.is_none() {
            // No operators found, treat as an operand
            return self.parse_operand();
        }

        let (op, index) = lowest_prec_operator.unwrap();
        let lhs = self.subslice(self.start,   index   ).parse_segment();
        let rhs = self.subslice(index+1,      self.end).parse_segment();

        self.parse_binary_operation(op, lhs, rhs)
    }

    fn parse_binary_operation(
        &self, 
        op: TokenType, lhs: Expression, rhs: Expression
    ) -> Expression {
        let base = reduce_assignment_operator(&op);
        match base {
            Some(o) => self.make_expr(
                ExpressionType::BinaryOp{lhs: lhs.clone(), op: TokenType::Assignment, rhs: 
                    self.make_expr(
                        ExpressionType::BinaryOp{lhs: lhs, op: o, rhs: rhs}
                    )
                }
            ),
            None => self.make_expr(
                ExpressionType::BinaryOp{lhs: lhs, op: op, rhs: rhs}
            )
        }
    }

    fn parse_operand(&self) -> Expression {
        // Operand is one token long, meaning it is an atom
        if self.len() == 1 {
            return self.parse_atom();
        }

        // Handle prefix operators (+, -)
        let prefix_op = self.parse_prefix_operation();
        if prefix_op.is_some() {
            return prefix_op.unwrap();
        }

        // Handle postfix operators
        let last = self.last();
        if [TokenType::BracketClose, TokenType::SquareBracketClose].contains(last) {
            let closing = match self.get_bracket_opening_index() {
                Some(i) => i,
                None    => return self.parsing_error("Mismatched brackets")
            };

            if closing == self.start {
                // Entire expression is in brackets
                return self.parse_brackets_expression();
            }

            // Otherwise it's an expression followed by brackets
            return self.parse_postfix_brackets_expression(closing);
        }

        // No prefix or postfix operations, also not an atom.
        // operand is a compound statement
        match self.first() {
            TokenType::If               => self.parse_if_expression(),
            TokenType::While            => self.parse_while_expression(),
            TokenType::BracketOpen      => self.parse_function_expression(),
            TokenType::For              => self.parse_for_expression(),
            TokenType::CurlyBracketOpen => self.parse_raw_scope_expression(),
            TokenType::EndLine          => {
                let mut err = self.parsing_error("Unexpected line end");
                err.start_line -= 1;
                err
            },
            _ => self.parsing_error("Unexpected token before compound statement")
        }
    }

    fn parse_atom(&self) -> Expression {
        macro_rules! make_literal { 
            ($e: expr) => { ExpressionType::Literal(Box::new($e)) }
        }
        let atom = match self.first() {
            TokenType::IntLiteral(i)    => make_literal!(IntObject::new(*i)),
            TokenType::FloatLiteral(f)  => make_literal!(FloatObject::new(*f)),
            TokenType::StringLiteral(s) => make_literal!(StringObject::new(s)),
            TokenType::CharLiteral(c)   => make_literal!(CharObject::new(*c)),
            TokenType::True             => make_literal!(BoolObject::new(true)),
            TokenType::False            => make_literal!(BoolObject::new(false)),
            TokenType::Nothing          => make_literal!(NothingObject::new()),
            TokenType::TypeInt          => make_literal!(TypeObject::Int),
            TokenType::TypeFloat        => make_literal!(TypeObject::Float),
            TokenType::TypeString       => make_literal!(TypeObject::Str),
            TokenType::TypeChar         => make_literal!(TypeObject::Char),
            TokenType::TypeBool         => make_literal!(TypeObject::Bool),
            TokenType::TypeNothing      => make_literal!(TypeObject::NothingType),
            TokenType::TypeList         => make_literal!(TypeObject::List),
            TokenType::TypeType         => make_literal!(TypeObject::TypeID),
            TokenType::TypeFunc         => make_literal!(TypeObject::Function),
            TokenType::Identifier(n)    => ExpressionType::Identifier(n.to_string()),
            TokenType::Break            => ExpressionType::Break,
            TokenType::Continue         => ExpressionType::Continue,
            TokenType::Return           => {
                let nothing = self.make_expr(make_literal!(NothingObject::new()));
                ExpressionType::Return(nothing)
            }
            _ => return self.parsing_error("Unexpected token")
        };
        return self.make_expr(atom);
    }

    fn parse_prefix_operation(&self) -> Option<Expression> {
        let operator = self.first();
        let parse_rest = || self
            .subslice(self.start+1, self.end)
            .parse_segment();

        let kind = match operator {
            TokenType::Add | TokenType::Subtract | 
            TokenType::Not | TokenType::BWNot
                => ExpressionType::UnaryOp {
                    op: operator.clone(), 
                    v: parse_rest()
                },
            TokenType::Return => ExpressionType::Return(parse_rest()),
            TokenType::Yield  => ExpressionType::Yield(parse_rest()),
            _ => return None
        };

        Some(self.make_expr(kind))
    }

    fn get_bracket_opening_index(&self) -> Option<usize> {
        // Called when the final token in the slice is a bracket
        // closing, returns the index of where this bracket was opened
        let closing = self.last();
        
        let mut latest_opener: Option<usize> = None;
        let mut bracket_stack                = BracketStack::new();
        
        for i in self.start..self.end {
            let tok        = self.get(i);
            let close      = BracketStack::get_matching_token(tok);
            // If the token is an opener and opens at the
            // lowest bracket level then set it to our opener
            if bracket_stack.is_empty() 
            && close.is_some()
            && &close.unwrap() == closing {
                latest_opener = Some(i);
            }
            // If we aren't in a bracket and the token is a closer
            // then there's more closers than openers.
            if bracket_stack.is_empty() && tok == closing {
                return None;
            }
            bracket_stack.evaluate_token(tok);
        }
        // If we're still in a bracket, there's more
        // openers than closers
        if !bracket_stack.is_empty() {
            return None;
        }

        latest_opener
    }

    fn parse_brackets_expression(&self) -> Expression {
        let closing = self.last();

        if self.start+1 == self.end-1 {
            // Brackets are empty
            let object: Box<dyn Object> = match closing {
                TokenType::BracketClose       
                    => Box::new(NothingObject::new()),
                TokenType::SquareBracketClose 
                    => Box::new(ListObject::new(Vec::new())),
                _ => panic!("parse_brackets_expression() called on non brackets")
            };
            return self.make_expr(ExpressionType::Literal(object))
        }

        // Brackets contain an expression, parse it.
        let inside = self
            .subslice(self.start+1, self.end-1)
            .parse_segment();

        match closing {
            TokenType::BracketClose       
                => inside,
            TokenType::SquareBracketClose 
                => self.make_expr(ExpressionType::ListConstruct(inside)),
            _                             
                => panic!("parse_brackets_expression()")
        }   
    }

    fn parse_postfix_brackets_expression(&self, open_index: usize) -> Expression {
        let operand = self.subslice(self.start, open_index).parse_segment();
        let inside  = if open_index+1 == self.end-1 {
            None
        } else {
            Some(self.subslice(open_index+1, self.end-1).parse_segment())
        };

        self.make_expr(match self.last() {
            TokenType::BracketClose => {
                ExpressionType::FunctionCall{ function: operand, args: inside }
            },
            TokenType::SquareBracketClose => {
                ExpressionType::Index{ operand: operand, index: match inside {
                    Some(v) => v,
                    None    => self.parsing_error(
                        "Expected expression between index brackets '[', ']'"
                    )
                } }
            },
            _ => panic!("parse_postfix_brackets_expression()")
        })
    }
    
    fn parse_while_expression(&self) -> Expression {
        let cond = match self.parse_conditional() {
            Ok(c)  => c,
            Err(e) => return e
        };
        self.make_expr(ExpressionType::While(cond))
    }

    fn parse_raw_scope_expression(&self) -> Expression {
        let scope = match self.parse_scoped_expression() {
            Ok(m)  => m,
            Err(e) => return e
        };
        self.make_expr(ExpressionType::RawScope(scope))
    }

    fn parse_for_expression(&self) -> Expression {
        let id = match self.get(self.start + 1) {
            TokenType::Identifier(s) => s,
            _ => return self.parsing_error(
                "Expected identifier"
            )
        };
        if let TokenType::In = self.get(self.start + 2) {
            let res = self
                .subslice(self.start + 2, self.end)
                .parse_block_with_arg();
            match res {
                Ok((e, b)) => self.make_expr(ExpressionType::For {
                    identifier: id.into(), 
                    indexable:  e,
                    body:       b
                }),
                Err(e) => e
            }
        } else {
            self.parsing_error("Expected 'in'")
        }
    }
    
    fn parse_function_expression(&self) -> Expression {
        let mut params          = Vec::<String>::new();
        let mut bracket_end     = self.start;
        let mut ready_for_param = true;

        for i in self.start+1..self.end {
            let tok = self.get(i);
            match tok {
            TokenType::Identifier(s) 
                => if ready_for_param {
                    params.push(s.to_string());
                    ready_for_param = false;
                } else {
                    return self.parsing_error("Expected ',' after parameter");
                },
            TokenType::Comma 
                => if !ready_for_param {
                    ready_for_param = true;
                } else {
                    return self.parsing_error("Expected parameter");
                },
            TokenType::BracketClose 
                => {
                    bracket_end = i+1;
                    break;
                },
            _
                => return self.parsing_error("Unexpected token in function expression")
            }
        }

        if bracket_end == self.start {
            return self.parsing_error(
                if params.len() == 0 {
                    "expected parameter or ')'"
                } else if ready_for_param {
                    "expected parameter"
                } else {
                    "expected ',' or ')'"
                }
            );
        }
        
        if self.get(bracket_end) != &TokenType::Arrow {
            return self.parsing_error("expected '->'");
        }

        let scope_open = bracket_end + 1;
        if scope_open == self.end 
        || self.get(scope_open) != &TokenType::CurlyBracketOpen {
            return self.parsing_error("expected '{'");
        }

        let res = self
            .subslice(scope_open, self.end)
            .parse_scoped_expression();

        let scope = match res {
            Ok(s)  => s,
            Err(e) => return e
        };

        self.make_expr(ExpressionType::Function{
            params: params,
            body: scope
        })

        /*
        let func = Function {
            parameters: params,
            body:       scope
        };

        let obj = ObjectPtr::Function(Box::new(func));

        self.make_expr(ExpressionType::Literal(obj))*/
    }

    fn parse_if_expression(&self) -> Expression {
        let mut bracket_stack = BracketStack::new();
        let mut branches      = Vec::<(usize, usize)>::new();
        let mut current_start = self.start;
        let mut prev_empty    = true;

        for i in self.start..self.end {
            let tok = self.get(i);
            bracket_stack.evaluate_token(tok);
            if bracket_stack.is_empty() && tok == &TokenType::CurlyBracketClose {
                if !prev_empty {
                    branches.push((current_start, i+1));
                    current_start = i+1;
                } 
                prev_empty = true;
            } else {
                prev_empty = false;
            }
        }

        let (if_start, if_end) = branches[0];
        let if_res = self.subslice(if_start, if_end).parse_conditional();
        let if_cond = match if_res {
            Ok(cond) => cond,
            Err(err) => return err
        };

        let mut elif_conds: Vec<   Conditional> = Vec::new();
        let mut else_cond:  Option<AST  > = None;
        let mut else_encountered = false;

        for i in 1..branches.len() {
            if else_encountered {
                return self.parsing_error("Unexpected token after 'else' block");
            }
            let (branch_start, branch_end) = branches[i];
            let branch_first = self.get(branch_start);
            let branch_slice = self.subslice(branch_start, branch_end);

            match branch_first {
            TokenType::Elif => {
                let elif_res = branch_slice
                    .parse_conditional();
                
                elif_conds.push(match elif_res {
                    Ok(cond) => cond,
                    Err(err) => return err
                });
            },
            TokenType::Else => {
                else_encountered = true;
                let scope_start = branch_start+1;
                let scope_first = self.get(scope_start);
                if scope_first != &TokenType::CurlyBracketOpen {
                    let err = self
                        .subslice(branch_start, scope_start+1)
                        .parsing_error("Expected '{' after 'else");
                    return err;
                }    
                else_cond = Some(self
                    .subslice(scope_start+1, branch_end-1)
                    .parse_multi_expr()
                );
            },
            _ => return branch_slice
                .parsing_error("Expected 'elif' or 'else' after 'if' branch")
            }
        }

        return self.make_expr( ExpressionType::If {
            if_:   if_cond, 
            elif:  elif_conds, 
            else_: else_cond
        });
    }
    
    fn parse_scoped_expression(&self) -> Result<AST, Expression> {
        let mut closing: Option<usize> = None;
        let mut bracket_stack = BracketStack::new();
        
        if self.first() != &TokenType::CurlyBracketOpen {
            panic!("attempted to parse unopened scope");
        }
        
        for i in self.start..self.end {
            let tok = self.get(i);
            bracket_stack.evaluate_token(tok);
            if bracket_stack.is_empty() {
                closing = Some(i);
                break;
            }
        }

        let closing_i = match closing {
            Some(i) => i,
            None    => return Err(self.parsing_error("Mismatched brackets"))
        };
        
        if closing_i != self.end-1 {
            return Err(self.parsing_error("Unexpected token after raw scope"));
        }
        
        let inside = self
        .subslice(self.start+1, self.end-1)
        .parse_multi_expr();
        
        Ok(inside)
    }

    fn parse_conditional(&self) -> Result<Conditional, Expression> {
        let (expr, scope) = self.parse_block_with_arg()?;
        Ok( Conditional {
            condition: expr,
            scope:     scope
        }) 
    }
    
    fn parse_block_with_arg(
        &self
    ) -> Result<(Expression, AST), Expression> {
        let mut bracket_stack = BracketStack::new();
        // find the location of the opening bracket of the conditional
        let mut opening: Option<usize> = None;

        for i in self.start+1..self.end {
            let tok = self.get(i);
            if bracket_stack.is_empty() && tok == &TokenType::CurlyBracketOpen {
                opening = Some(i);
                break;
            }
            bracket_stack.evaluate_token(tok);
        }

        let opening_location = match opening {
            Some(i) => i,
            None    => return Err(self.parsing_error(
                "Expected '{'"
            ))
        };

        if opening_location == self.start+1 {
            return Err(self.parsing_error(
                "Expected expression"
            ));
        }

        let condition = self
            .subslice(self.start+1, opening_location)
            .parse_segment();

        let scope_parse_res = self
            .subslice(opening_location, self.end)
            .parse_scoped_expression();

        let scope = match scope_parse_res {
            Ok(m)  => m,
            Err(e) => return Err(e)
        };

        Ok( ( condition, scope ) )
    }
}
