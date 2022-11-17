// 
// debug.rs 
// Contains functions for printing tokenised and parsed
// structures for debugging. 
//
use crate::parse::{Expression, ExpressionType, Conditional, AST};
use crate::lex::Token;
use std::fmt::Debug;

pub fn print_tokens(toks: &Vec<Token>) {
    println!("Lexing results:");
    for tok in toks {
        println!(
            "{:?} on line {} ({} - {})", 
            tok.kind, tok.line, tok.start, tok.end);
    }
}

pub fn print_multi_expression(exprs: &AST) {
    println!("Parsing Results:");
    for e in exprs {
        print_expression(e);
    }
}

fn print_expression(expr: &Expression) {
    let start = TreeLevel{ padding_level: 0 };
    start.print(expr);
}

struct TreeLevel {
    padding_level: usize
}

impl TreeLevel {
    fn next(&self) -> Self {
        Self{padding_level: self.padding_level+1}
    }

    fn print_padding(&self) { 
        Self::print_padding_by_level(self.padding_level); 
    }

    fn last_padding(&self) {
        Self::print_padding_by_level(self.padding_level-1);
    }

    fn print_padding_by_level(l: usize) {
        for _ in 0..l {
            print!("  | ");
        }
    }

    fn expression<T: Debug>(&self, label: T, expr: &Expression) {
        println!("{:?}", label);
        self.print(expr);
    }

    fn multi_expression<T: Debug>(&self, label: T, exprs: &AST) {
        println!("{:?}", label);
        for expr in exprs {
            self.print(&expr);
        }
    }

    fn conditional<T: Debug>(&self, label: T, cond: &Conditional) {
        self.expression(label, &cond.condition);
        self.last_padding();
        self.multi_expression("do", &cond.scope);
    }

    fn print(&self, expr: &Expression) {
        self.print_padding();

        let next = self.next();
        let inner_type = expr.kind();

        match inner_type {
            ExpressionType::Identifier(s)    => println!("identifier {}", s),
            ExpressionType::RawScope(exprs)  => next.multi_expression("scope", exprs),
            ExpressionType::While(cond)      => next.conditional("while", &cond),
            ExpressionType::ListConstruct(l) => next.expression("list", &l),
            ExpressionType::Return(e)        => next.expression("return", &e),
            ExpressionType::Yield(e)         => next.expression("yield", &e),
            ExpressionType::Break            => println!("break"),
            ExpressionType::Continue         => println!("continue"),
            ExpressionType::Error(s)         => println!("Error: {}", s),
            ExpressionType::UnaryOp{op, v}   => next.expression(op, &v),
            ExpressionType::BinaryOp{lhs, op, rhs} => {
                println!("{:?}", op);
                next.print(&lhs);
                next.print(&rhs);
            }
            ExpressionType::If{if_, elif, else_} => {
                next.conditional("if", &if_);
                for elif_i in elif {
                    self.print_padding();
                    next.conditional("elif", &elif_i);
                }
                if else_.is_some() {
                    self.print_padding();
                    next.multi_expression("else do", &else_.as_ref().unwrap());
                }
            },
            ExpressionType::For{identifier, indexable, body} => {
                println!("for {} in", identifier);
                next.print(&indexable);
                self.print_padding();
                next.multi_expression("do", &body);
            },
            ExpressionType::Index{operand, index} => {
                next.expression("list expression", &operand);
                self.print_padding();
                next.expression("indexed by", &index);
            },
            ExpressionType::Function{params, body} => {
                println!("function with parameters");
                for param in params {
                    next.print_padding();
                    println!("{}", param);
                }
                self.print_padding();
                next.multi_expression("with body", &body);
            },
            ExpressionType::FunctionCall{function, args} => {
                next.expression("function call", &function);
                self.print_padding();
                match args {
                    Some(a) => next.expression("with args", &a),
                    None    => println!("with no args")
                }
            }
            ExpressionType::Literal(o)  => {
                println!(
                    "literal {} : {}", 
                    o.typename(),
                    o.to_string());
            }
        }
    }
}