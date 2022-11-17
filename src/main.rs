use std::fs;
use std::env;
use std::rc::Rc;

pub mod bracketstack;
pub mod lex;
pub mod parse;
pub mod error;
pub mod object;
pub mod debug;
pub mod function;
pub mod run;

const DEBUG: bool = false;

fn main() {
    let args     = env::args().collect::<Vec<String>>();
    let num_args = args.len();
    if num_args == 1 {
        println!("Usage: {} [filename]", &args[0]);
        return;
    } 

    // Read the contents of the file
    let filename = &args[1];
    let file_contents = fs::read_to_string(filename);
    if file_contents.is_err() {
        println!("Error No such file: {}", filename);
        return;
    }
    let source = file_contents.unwrap();

    // Lexing
    let tokens = lex::tokenise(&source);
    if DEBUG {
        debug::print_tokens(&tokens)
    }
    if error::handle_lexing_errors(&source, &tokens) {
        return;
    }

    // Parsing
    let ast = parse::parse(&tokens);
    if DEBUG {
        debug::print_multi_expression(&ast);
    }
    if error::handle_parsing_errors(&source, &ast) {
        return;
    }

    // Evaluating
    let source_ptr = Rc::new(source);
    let runtime = run::Interpreter::new(&source_ptr);
    runtime.evaluate_ast(&ast);
}
