// 
// error.rs
// Contains functions to locate and print errors
//

use crate::lex::{Token, TokenType};
use crate::parse::{Expression, ExpressionType, AST};

pub fn handle_lexing_errors(source: &str, tokens: &Vec<Token>) -> bool {
    let mut a = ErrorCollector::new(source);
    a.find_token_errors(tokens);
    a.print_errors_if_found()
}

pub fn handle_parsing_errors(source: &str, ast: &AST) -> bool {
    let mut a = ErrorCollector::new(source);
    a.find_multi_expression_errors(ast);
    a.print_errors_if_found()
}

pub fn handle_evaluating_error(source: &str, expr: &Expression, msg: &str) {
    let mut a = ErrorCollector::new(source);
    a.add_error(
        "runtime", 
        expr.start_line,  expr.end_line,
        expr.start_index, expr.end_index,
        msg);
    a.print_errors_if_found();
}

struct Error {
    pub stage:       &'static str,
    pub line_start:  usize, 
    pub line_end:    usize, 
    pub index_start: usize, 
    pub index_end:   usize, 
    pub message:     String
}

struct ErrorCollector<'a> {
    source: &'a str,
    errors: Vec<Error>
}

impl<'a> ErrorCollector<'a> {
    pub fn new(source: &'a str) -> Self { Self {
        source: source,
        errors: Vec::new()
    }}

    pub fn print_errors_if_found(&self) -> bool {
        const RED:   &str = "\x1b[31m";
        const GREY:  &str = "\x1b[37m";
        const RESET: &str = "\x1b[0m";
    
        fn num_digits(x: usize) -> usize {
            let f = (x + 1) as f64;
            f.log10().ceil() as usize
        }

        if self.errors.len() == 0 {
            return false;
        }

        let lines: Vec<&str> = self.source.split("\n").collect();
    
        for err in &self.errors {
            println!("{} error encountered:", err.stage);

            let max_width = num_digits(err.line_end);
            let padding = " ".repeat(max_width);
            println!("{} {} |{}", GREY, padding, RESET);

            let mut in_red = false;
    
            for i in err.line_start-1..err.line_end {
                let line_no = i+1;
                let digit_d = max_width - num_digits(line_no);
                let extra_padding = " ".repeat(digit_d);
                print!("{} {}{} | {}", GREY, line_no, extra_padding, RESET);
                let line = lines[i];

                let mut red_started = false;

                for (j, c) in line.chars().enumerate() {
                    if j == err.index_start && i == err.line_start-1 { 
                        in_red = true
                    } else if j == err.index_end && i == err.line_end-1 { 
                        in_red = false
                    }

                    if in_red && !red_started {
                        print!("{}", RED); 
                        red_started = true;
                    } else if !in_red && red_started {
                        print!("{}", RESET);
                        red_started = false;
                    }

                    print!("{}", c);
                }

                print!("{}", RESET);
                print!("\n");
            }
        
            println!("{} {} |{}", GREY, padding, RESET);
            println!("{}\n", err.message);
        }

        true
    }

    pub fn find_token_errors(&mut self, toks: &Vec<Token>) {
        for t in toks {
            if let TokenType::Error(s) = &t.kind {
                self.add_error("lexing", t.line, t.line, t.start, t.end, &s);
            }
        }
    }

    pub fn find_multi_expression_errors(&mut self, exprs: &AST) {
        for e in exprs {
            self.find_expression_errors(&e);
        }
    }

    fn find_expression_errors(&mut self, expr: &Expression) {
        macro_rules! p { ($l:expr) => { self.find_expression_errors($l); } }
        macro_rules! m { ($l:expr) => { self.find_multi_expression_errors($l); } }
        match expr.kind() {
            ExpressionType::Yield         (s) => { p!(&s);                         },
            ExpressionType::Return        (s) => { p!(&s);                         },
            ExpressionType::RawScope      (s) => { m!(&s);                         },
            ExpressionType::ListConstruct (s) => { p!(&s);                         },
            ExpressionType::While         (s) => { p!(&s.condition); m!(&s.scope); },
            ExpressionType::UnaryOp  {op:_, v}        => { p!(&v);                   },
            ExpressionType::Function {params:_, body} => { m!(&body);                },
            ExpressionType::BinaryOp {lhs, op:_, rhs} => { p!(&lhs);     p!(&rhs);   },
            ExpressionType::Index    {operand, index} => { p!(&operand); p!(&index); },
            ExpressionType::FunctionCall {function, args} => {
                p!(function);
                if let Some(ex) = args {
                    p!(ex);
                }
            },
            ExpressionType::If {if_, elif, else_} => {
                p!(&if_.condition);
                m!(&if_.scope);
                for cond in elif {
                    p!(&cond.condition);
                    m!(&cond.scope);
                }
                if let Some(cond) = else_ {
                    m!(&cond);
                }
            },
            ExpressionType::For {identifier: _, indexable, body} => {
                p!(indexable);
                m!(body);
            },
            ExpressionType::Error (s) => {
                self.add_error(
                    "parsing", 
                    expr.start_line,  expr.end_line,
                    expr.start_index, expr.end_index,
                    s);
            },
            _ => {}
        }
    }

    pub fn add_error(
        &mut self,
        stage:       &'static str,
        line_start:  usize, 
        line_end:    usize, 
        index_start: usize, 
        index_end:   usize, 
        message:     &str
    ) {
        self.errors.push( Error {
            stage:       stage,
            line_start:  line_start, 
            line_end:    line_end, 
            index_start: index_start, 
            index_end:   index_end, 
            message:     message.to_string()
        });
    }
}

/*
pub struct ErrorPrinter<'a> {
    source: &'a str,
    errors: Vec<Error>
}

pub fn print_runtime_error(source: &str, expr: &Expression, msg: &str) {
    let mut printer = ErrorPrinter::new(source);
    printer.add_error(
        "runtime", 
        expr.start_line,  expr.end_line,
        expr.start_index, expr.end_index,
        msg);
    printer.print_errors();
}

impl<'a> ErrorPrinter<'a> {
    pub fn new(source: &'a str) -> Self { Self {
        source: source,
        errors: Vec::new()
    }}

    pub fn errors(&self) -> &Vec<Error> {
        &self.errors
    }

    pub fn print_errors(&self) {
        const RED:   &str = "\x1b[31m";
        const GREY:  &str = "\x1b[37m";
        const RESET: &str = "\x1b[0m";

        fn num_digits(x: usize) -> usize {
            let f = (x + 1) as f64;
            f.log10().ceil() as usize
        }    

        let count = self.errors.len();

        if count == 0 {
            panic!("print_errors() should only be called when errors are found");
        }

        let lines: Vec<&str> = self.source.split("\n").collect();

        for err in &self.errors {
            let max_width = num_digits(err.line_end);
            let padding = " ".repeat(max_width);
            println!("{} {} |{}", GREY, padding, RESET);

            for i in err.line_start-1..err.line_end {
                let line_no = i+1;
                let digit_d = max_width - num_digits(line_no);
                let extra_padding = " ".repeat(digit_d);
                print!("{} {}{} | {}", GREY, line_no, extra_padding, RESET);
                let line = lines[i];
                for (i, c) in line.chars().enumerate() {
                    if      i == err.index_start { print!("{}", RED); }
                    else if i == err.index_end   { print!("{}", RESET); }
                    print!("{}", c);
                }
                print!("\n");
            }
        
            println!("{} {} |{}", GREY, padding, RESET);
            println!("{}\n", err.message);
        }
    }

    pub fn runtime_error(&mut self, expr: &Expression, msg: &str) {

    }
    
    pub fn find_lexer_errors(&mut self, toks: &Vec<Token>) -> bool {
        for t in toks {
            if let TokenType::Error(s) = &t.kind {
                self.add_error("lexing", t.line, t.line, t.start, t.end, &s);
            }
        }
        self.errors.len() > 0
    }

    pub fn find_parser_errors(&mut self, ast: &AST) -> bool {
        self.find_multi_expr_errors(ast);
        self.errors.len() > 0
    }

    fn find_multi_expr_errors(&mut self, multi_expr: &AST) {
        for e in multi_expr {
            self.find_expr_errors(&e);
        }
    }
    
    fn find_expr_errors(&mut self, expr: &Expression) {
        macro_rules! p { ($l:expr) => { self.find_expr_errors($l); } }
        macro_rules! m { ($l:expr) => { self.find_multi_expr_errors($l); } }
        match expr.kind() {
            ExpressionType::Yield         (s) => { p!(&s);                         },
            ExpressionType::Return        (s) => { p!(&s);                         },
            ExpressionType::RawScope      (s) => { m!(&s);                         },
            ExpressionType::ListConstruct (s) => { p!(&s);                         },
            ExpressionType::While         (s) => { p!(&s.condition); m!(&s.scope); },
            ExpressionType::UnaryOp  {op:_, v}        => { p!(&v);                   },
            ExpressionType::Function {params:_, body} => { m!(&body);                },
            ExpressionType::BinaryOp {lhs, op:_, rhs} => { p!(&lhs);     p!(&rhs);   },
            ExpressionType::Index    {operand, index} => { p!(&operand); p!(&index); },
            ExpressionType::FunctionCall {function, args} => {
                p!(function);
                if let Some(ex) = args {
                    p!(ex);
                }
            },
            ExpressionType::If {if_, elif, else_} => {
                p!(&if_.condition);
                m!(&if_.scope);
                for cond in elif {
                    p!(&cond.condition);
                    m!(&cond.scope);
                }
                if let Some(cond) = else_ {
                    m!(&cond);
                }
            },
            ExpressionType::Error (s) => {
                self.add_error(
                    "parsing", 
                    expr.start_line,  expr.end_line,
                    expr.start_index, expr.end_index,
                    s);
            },
            _ => {}
        }
    }

    pub fn add_error(
        &mut self,
        stage:       &'static str,
        line_start:  usize, 
        line_end:    usize, 
        index_start: usize, 
        index_end:   usize, 
        message:     &str
    ) {
        self.errors.push( Error {
            stage:       stage,
            line_start:  line_start, 
            line_end:    line_end, 
            index_start: index_start, 
            index_end:   index_end, 
            message:     message.to_string()
        });
    }
}
*/