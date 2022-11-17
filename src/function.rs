//
// function.rs
// Implements functions
//
use crate::parse::AST;
use crate::object::*;
use crate::run::{Interpreter, ScopeType};
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io;
use std::io::Write;

pub trait Function {
    fn call(&self, args: &Vec<ObjectPtr>) -> ObjectRes;
}

//
// Source code function
//
pub struct ScriptFunction {
    pub source:     Rc<String>,
    pub parameters: Vec<String>,
    pub state:      HashMap<String, ObjectSharedPtr>,
    pub body:       AST
}

impl Function for ScriptFunction {
    fn call(&self, args: &Vec<ObjectPtr>) -> ObjectRes {
        if args.len() != self.parameters.len() {
            return Err(format!(
                "This function requires {} arguments ({}), got {}",
                self.parameters.len(),
                self.parameters.join(", "),
                args.len()));
        }
        
        let mut arg_state = HashMap::<String, ObjectSharedPtr>::new();

        for (val, key) in args.iter().zip(self.parameters.iter()) {
            let ptr = Rc::new(RefCell::new(val.copy_object()));
            arg_state.insert(key.into(), ptr);
        }

        let mut our_state = self.state.clone();
        our_state.extend(arg_state);

        let interp = Interpreter::make(&self.source, ScopeType::Function, our_state);
        Ok(interp
            .evaluate_ast(&self.body)
            .as_object())
    }
}

// 
// Builtin system functions 
//
pub struct SystemPrint {
    pub newline: bool
}

impl Function for SystemPrint {
    fn call(&self, args: &Vec<ObjectPtr>) -> ObjectRes {
        if args.len() == 0 {
            return Err("This function requires more than one argument".into());
        }
        for arg in args {
            print!("{}", arg.to_string());
            if self.newline {
                print!("\n");
            }
        }
        Ok(Box::new(NothingObject::new()))
    }
}

pub struct SystemReadInput {}

impl Function for SystemReadInput {
    fn call(&self, args: &Vec<ObjectPtr>) -> ObjectRes {
        let prompt = match args.len() {
            0 => "".to_string(),
            1 => args[0].to_string(),
            _ => return Err("This function takes 0 or 1 argument".into())
        };
        print!("{}", prompt);
        std::io::stdout().flush().expect("Could not flush stdout");

        let mut input = String::new();
        let res = io::stdin().read_line(&mut input);

        match res {
            Ok(_)    => Ok(Box::new(StringObject::new(input.trim()))),
            Err(msg) => Err(format!("Failed to read input because: {}", msg))
        }
    }
}

pub struct SystemObjectLength {}

impl Function for SystemObjectLength {
    fn call(&self, args: &Vec<ObjectPtr>) -> ObjectRes {
        if args.len() != 1 {
            return Err("This function requires only one argument".into());
        }
        let obj = &args[0];
        let length = obj.get_length();
        match length {
            Some(l) => Ok(Box::new(IntObject::new(l as i64))),
            None    => Err(format!("Cannot get length of {}", obj.typename()))
        }
    }
}