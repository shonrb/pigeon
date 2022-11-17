//
// run.rs
// Evaluates an abstract syntax tree
//
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use crate::object::*;
use crate::parse::{Expression, AST, ExpressionType, Conditional};
use crate::error::handle_evaluating_error;
use crate::lex::TokenType;
use crate::function::*;

#[derive(PartialEq)]
pub enum ScopeType {
    File,
    Function,
    SingleBlock,
    LoopBlock
}

pub enum ReturnState {
    Return(ObjectPtr),
    Yield(ObjectPtr),
    Break,
    Continue,
    None
}

pub struct Interpreter {
    source:           Rc<String>,
    kind:             ScopeType,
    variables:        HashMap<String, ObjectSharedPtr>,
    expression_stack: Vec<Expression>,
    returned:         bool,
    return_state:     ReturnState
}

impl ReturnState {
    pub fn as_object(&self) -> ObjectPtr {
        match self {
            Self::Return(o) => o.copy_object(),
            Self::Yield(o)  => o.copy_object(),
            _               => Box::new(NothingObject::new())
        }
    }
}

impl Interpreter {
    pub fn new(src: &Rc<String>) -> Self {
        let mut a = Self::make(src, ScopeType::File, HashMap::new());
        a.initialise_builtin_functions();
        a
    }

    pub fn make(
        src: &Rc<String>, 
        t: ScopeType,
        h: HashMap<String, ObjectSharedPtr>
    ) -> Self { 
        Self {
            source:           Rc::clone(src),
            kind:             t,
            variables:        h,
            expression_stack: Vec::new(),
            returned:         false,
            return_state:     ReturnState::None
        }
    }

    pub fn initialise_builtin_functions(&mut self) {
        macro_rules! make_function { ($f: expr) => { 
            Rc::new(
            RefCell::new(
            FunctionObject::new(
            Rc::new($f)).copy_object()))}}
        let builtin_functions = [
            ("print", make_function!(SystemPrint{ newline: false })),
            ("println", make_function!(SystemPrint{ newline: true })),
            ("len",   make_function!(SystemObjectLength{})),
            ("input", make_function!(SystemReadInput{}))
        ];
        for (name, func) in builtin_functions {
            self.variables.insert(name.into(), func);
        }
    }

    pub fn make_subscope(&self, kind: ScopeType) -> Self {
        Self::make(&self.source, kind, self.variables.clone())
    }

    pub fn set_variable(&mut self, name: &str, value: ObjectSharedPtr) {
        self.variables.insert(name.into(), value);
    }
    
    fn get_variable(&mut self, name: &str, create_if_not_found: bool) -> RuntimeValue {
        let ret = self.variables.get(name);
        match ret {
            Some(object) => {
                let ptr = Rc::clone(object);
                RuntimeValue::new_perm_from_ptr(ptr)
            },
            None => if create_if_not_found {
                let nothing = NothingObject::new().copy_object();
                let ptr     = Rc::new(RefCell::new(nothing));
                self.variables.insert(name.to_string(), Rc::clone(&ptr)); 
                RuntimeValue::new_perm_from_ptr(ptr)
            } else {
                self.die("This variable does not exist")
            }
        }
    }

    pub fn evaluate_ast(mut self, exp: &AST) -> ReturnState {
        for e in exp {
            self.evaluate_expression(e);
            if self.returned {
                break;
            }
        }
        self.return_state
    }

    fn evaluate_expression_base(&mut self, exp: &Expression, create: bool) -> RuntimeValue {
        self.expression_stack.push(exp.clone());
        let res = match exp.kind() {
            ExpressionType::Literal(obj)                    
                => RuntimeValue::new_temp(obj.copy_object()),
            ExpressionType::Identifier(s)                   
                => self.get_variable(s, create),
            ExpressionType::BinaryOp { lhs, op, rhs }       
                => self.evaluate_binary_op(lhs, op, rhs),
            ExpressionType::UnaryOp { op, v }               
                => self.evaluate_unary_op(op, v),
            ExpressionType::FunctionCall { function, args } 
                => self.evaluate_function_call(function, args),
            ExpressionType::ListConstruct(exp) 
                => self.evaluate_list_construction(exp),
            ExpressionType::Index { operand, index }        
                => self.evaluate_index(operand, index),
            ExpressionType::Function { params, body }       
                => self.evaluate_function_def(params, body),
            ExpressionType::Return(expr)
                => self.evaluate_return(expr),
            ExpressionType::Yield(expr)                     
                => self.evaluate_yield(expr),
            ExpressionType::Break
                => self.evaluate_break(),
            ExpressionType::Continue
                => self.evaluate_continue(),
            ExpressionType::RawScope(exprs) 
                => self.evaluate_raw_scope(exprs),
            ExpressionType::If { if_, elif, else_ }         
                => self.evaluate_if(if_, elif, else_),
            ExpressionType::While(cond)                     
                => self.evaluate_while(cond),
            ExpressionType::For { identifier, indexable, body }
                => self.evaluate_for(identifier, indexable, body),
            ExpressionType::Error(_) => panic!("tried to run an ast with errors")
        };
        self.expression_stack.pop();
        res
    }

    pub fn evaluate_expression(&mut self, exp: &Expression) -> RuntimeValue {
        self.evaluate_expression_base(exp, false)
    }
    
    pub fn evaluate_expression_init_identifier(&mut self, exp: &Expression) -> RuntimeValue {
        self.evaluate_expression_base(exp, true)
    }
    
    fn evaluate_binary_op(
        &mut self, lhs: &Expression, op: &TokenType, rhs: &Expression
    ) -> RuntimeValue {
        if op == &TokenType::Assignment {
            let lhs_val = self.evaluate_expression_init_identifier(lhs);
            let rhs_val = self.evaluate_expression(rhs);
            return self.evaluate_assignment(lhs_val, rhs_val);
        }
        let lhs_id = self.evaluate_expression(lhs);
        let rhs_id = self.evaluate_expression(rhs);
        let lhs_obj = lhs_id.object.borrow();
        let rhs_obj = rhs_id.object.borrow();

        let res = match op {
            TokenType::Add                => lhs_obj.operator_add(&rhs_obj),
            TokenType::Subtract           => lhs_obj.operator_sub(&rhs_obj),
            TokenType::Multiply           => lhs_obj.operator_mul(&rhs_obj),
            TokenType::Divide             => lhs_obj.operator_div(&rhs_obj),
            TokenType::Mod                => lhs_obj.operator_mod(&rhs_obj),
            TokenType::Or                 => lhs_obj.operator_or(&rhs_obj),
            TokenType::And                => lhs_obj.operator_and(&rhs_obj),
            TokenType::Equality           => lhs_obj.operator_eq(&rhs_obj),
            TokenType::Inequality         => lhs_obj.operator_neq(&rhs_obj),
            TokenType::LessThan           => lhs_obj.operator_lt(&rhs_obj),
            TokenType::GreaterThan        => lhs_obj.operator_gt(&rhs_obj),
            TokenType::LessThanOrEqual    => lhs_obj.operator_lt_eq(&rhs_obj),
            TokenType::GreaterThanOrEqual => lhs_obj.operator_gt_eq(&rhs_obj),
            TokenType::BWOr               => lhs_obj.operator_bw_or(&rhs_obj),
            TokenType::BWAnd              => lhs_obj.operator_bw_and(&rhs_obj),
            TokenType::BWXor              => lhs_obj.operator_bw_xor(&rhs_obj),
            TokenType::BWShiftLeft        => lhs_obj.operator_bw_shiftl(&rhs_obj),
            TokenType::BWShiftRight       => lhs_obj.operator_bw_shiftr(&rhs_obj),
            TokenType::To                 => return self.evaluate_range(&lhs_obj, &rhs_obj),
            TokenType::Comma              => return self.die("Unexpected comma"),
            _ => panic!("Not implemented")
        };

        match res {
            Ok(obj)  => RuntimeValue::new_temp(obj),
            Err(msg) => self.die(&msg)
        }
    }

    fn evaluate_range(&mut self, lhs: &ObjectPtr, rhs: &ObjectPtr) -> RuntimeValue {
        if lhs.get_type() != TypeObject::Int 
        || rhs.get_type() != TypeObject::Int {
            self.die(&format!(
                "range requires types int and int, not {} and {}",
                rhs.typename(), lhs.typename()));
        }

        let mut objects = Vec::<ObjectPtr>::new();

        for i in lhs.get_int().unwrap() .. rhs.get_int().unwrap() {
            let obj: ObjectPtr = Box::new(IntObject::new(i));
            objects.push(obj);
        }

        RuntimeValue::new_temp(Box::new(ListObject::new(objects)))
    }
    
    fn evaluate_assignment(&mut self, lhs: RuntimeValue, rhs: RuntimeValue) -> RuntimeValue {
        if lhs.temporary {
            return self.die("Cannot assign to this value");
        }
        let val = rhs.object.borrow().copy_object();
        lhs.object.replace(val.copy_object());
        RuntimeValue::new_temp(val)
    }

    fn evaluate_unary_op(&mut self, operator: &TokenType, value: &Expression) -> RuntimeValue {
        let value_res = self.evaluate_expression(value);
        let value_obj = value_res.object.borrow();
        let res = match operator {
            TokenType::Add      => value_obj.operator_pos(),
            TokenType::Subtract => value_obj.operator_neg(),
            TokenType::Not      => value_obj.operator_not(),
            TokenType::BWNot    => value_obj.operator_bw_not(),
            _ => panic!("Not implemented")
        };
        match res {
            Ok(obj)  => RuntimeValue::new_temp(obj),
            Err(msg) => self.die(&msg)
        }
    }

    fn evaluate_function_call(&mut self, subject: &Expression, args: &Option<Expression>) -> RuntimeValue {
        let flat_args = match args {
            Some(e) => Self::flatten_comma_expression(e),
            None    => Vec::new()
        };

        let subject_res  = self.evaluate_expression(subject);
        let subject_obj = subject_res.object.borrow().copy_object();

        let mut args = Vec::<ObjectPtr>::new();

        for a in flat_args {
            let expr = self.evaluate_expression(a);
            args.push(expr.object.borrow().copy_object());
        }

        let res = subject_obj.call(&args);
        match res {
            Ok(o)    => RuntimeValue::new_temp(o),
            Err(msg) => self.die(&msg)
        }
    }

    fn evaluate_list_construction(&mut self, args: &Expression) -> RuntimeValue {
        let flat_args = Self::flatten_comma_expression(args);
        let mut args = Vec::<ObjectPtr>::new();
        for a in flat_args {
            let expr = self.evaluate_expression(a);
            args.push(expr.object.borrow().copy_object());
        }
        let obj = Box::new(ListObject::new(args));
        RuntimeValue::new_temp(obj)
    }

    fn flatten_comma_expression(expr: &Expression) -> Vec<&Expression> {
        if let ExpressionType::BinaryOp {lhs, op, rhs} = expr.kind() {
            if op == &TokenType::Comma {
                let mut v = Vec::new();
                v.append(&mut Self::flatten_comma_expression(lhs));
                v.push(rhs);
                return v;
            }
        }
        vec!(expr)
    }

    fn evaluate_index(&mut self, operand: &Expression, index: &Expression) -> RuntimeValue {
        let lhs = self.evaluate_expression(operand).object;
        let rhs = self.evaluate_expression(index).object;
        let res = lhs.borrow().index(&rhs.borrow());
        match res {
            Ok(o)    => o,
            Err(msg) => self.die(&msg)
        }
    }

    fn evaluate_function_def(&mut self, params: &Vec<String>, body: &AST) -> RuntimeValue {
        let func  = ScriptFunction {
            source:     self.source.clone(),
            parameters: params.clone(),
            state:      self.variables.clone(),
            body:       body.clone()
        };
        let obj = Box::new(FunctionObject::new(Rc::new(func)));
        RuntimeValue::new_temp(obj)
    }

    fn evaluate_return(&mut self, expr: &Expression) -> RuntimeValue {
        self.evaluate_generic_return_expr(expr, |x| ReturnState::Return(x))
    }

    fn evaluate_yield(&mut self, expr: &Expression) -> RuntimeValue {
        if self.kind == ScopeType::Function || self.kind == ScopeType::File {
            self.die("Cannot yield at function or file scope");
        }
        self.evaluate_generic_return_expr(expr, |x| ReturnState::Yield(x))
    }

    fn evaluate_break(&mut self) -> RuntimeValue {
        self.set_return_state(ReturnState::Break);
        RuntimeValue::new_temp(Box::new(NothingObject::new()))
    }

    fn evaluate_continue(&mut self) -> RuntimeValue {
        self.set_return_state(ReturnState::Continue);
        RuntimeValue::new_temp(Box::new(NothingObject::new()))
    }

    fn evaluate_generic_return_expr(
        &mut self, expr: &Expression, make: fn(ObjectPtr) -> ReturnState
    ) -> RuntimeValue {
        let result = self.evaluate_expression(expr).object;
        let object = result.borrow();
        self.set_return_state(make(object.copy_object()));
        RuntimeValue::new_temp(object.copy_object())
    }

    fn evaluate_raw_scope(&mut self, exprs: &AST) -> RuntimeValue {
        let ret = self
            .make_subscope(ScopeType::SingleBlock)
            .evaluate_ast(exprs);
        self.handle_subscope_return_state(ret)
    }

    fn evaluate_if(
        &mut self, 
        if_:   &Conditional, 
        elif:  &Vec<Conditional>, 
        else_: &Option<AST>
    ) -> RuntimeValue {
        macro_rules! return_if_cond { ($c: expr, $s: expr) => {
            let res = self.eval_conditional($c, ScopeType::SingleBlock, $s);
            if let Some(val) = res {
                return self.handle_subscope_return_state(val);
            }
        }}
        return_if_cond!(if_, "if");
        for e in elif {
            return_if_cond!(e, "elif");
        }
        if let Some(e) = else_ {
            return self.evaluate_raw_scope(e);
        }
        RuntimeValue::new_temp(Box::new(NothingObject::new()))
    }

    fn evaluate_while(&mut self, cond: &Conditional) -> RuntimeValue {
        let mut rs = Vec::<ObjectPtr>::new();
        loop {
            let res = self.eval_conditional(cond, ScopeType::LoopBlock, "while");
            if res.is_none() {
                break;
            }
            let returnval = res.unwrap();
            match (&self.kind, &returnval) {
                (_,               ReturnState::Yield(o) ) => rs.push(o.copy_object()),
                (ScopeType::File, ReturnState::Return(_)) => return self.die("Cannot return at file scope"),
                (_,               ReturnState::Return(_)) => self.set_return_state(returnval),
                (_,               ReturnState::Break)     => break,
                (_,               ReturnState::Continue)  => continue,
                _ => {}
            };
        }
        RuntimeValue::new_temp(
            if rs.len() > 0 {
                Box::new(ListObject::new(rs))
            } else {
                Box::new(NothingObject::new())
            }
        )
    }

    fn evaluate_for(&mut self, identifier: &str, indexable: &Expression, body: &AST) -> RuntimeValue {
        let mut rs = Vec::<ObjectPtr>::new();
        let iter = self.evaluate_expression(indexable);
        let obj  = iter.object.borrow();
        let len = match obj.get_length() {
            Some(l) => l,
            None    => return self.die(&format!(
                "Objects of type {} have no length", 
                obj.typename()
            ))
        };
        for i in 0..len {
            let as_intobj: ObjectPtr = Box::new(IntObject::new(i as i64));
            let for_object = match obj.index(&as_intobj) {
                Ok(o)    => o,
                Err(msg) => return self.die(&msg)
            };
            let ptr = for_object.object.clone();
            let mut scope = Interpreter::make(&self.source, ScopeType::LoopBlock, self.variables.clone());
            scope.set_variable(identifier, ptr);
            let returnval = scope.evaluate_ast(body);
            match (&self.kind, &returnval) {
                (_,               ReturnState::Yield(o) ) => rs.push(o.copy_object()),
                (ScopeType::File, ReturnState::Return(_)) => return self.die("Cannot return at file scope"),
                (_,               ReturnState::Return(_)) => self.set_return_state(returnval),
                (_,               ReturnState::Break)     => break,
                (_,               ReturnState::Continue)  => continue,
                _ => {}
            };
        }
        RuntimeValue::new_temp(
            if rs.len() > 0 {
                Box::new(ListObject::new(rs))
            } else {
                Box::new(NothingObject::new())
            }
        )
    }

    fn eval_conditional(
        &mut self, 
        cond:       &Conditional, 
        scope_type: ScopeType,
        name:       &str
    ) -> Option<ReturnState> {
        let condition = {
            let res = self.evaluate_expression(&cond.condition);
            let rf  = res.object.borrow();
            rf.copy_object()
        };
        if condition.get_type() != TypeObject::Bool {
            self.die(&format!(
                "{} conditions must be bool, not {}", 
                name,
                condition.typename()));
        }
        let as_bool = condition.get_bool().unwrap();
        if as_bool {
            Some(self
                .make_subscope(scope_type)
                .evaluate_ast(&cond.scope))
        } else {
            None
        }
    }

    fn handle_subscope_return_state(&mut self, state: ReturnState) -> RuntimeValue {
        let object = state.as_object();
        match &state {
            ReturnState::Return(_) => // Propagates until function scope
                if self.kind == ScopeType::File { 
                    self.die("Can only return from functions");
                } else {
                    self.set_return_state(state);
                },
            ReturnState::Break | 
            ReturnState::Continue => // Propagates until loop scope
                if self.kind == ScopeType::LoopBlock 
                || self.kind == ScopeType::SingleBlock {
                    self.set_return_state(state);
                } else {
                    self.die("break or continue is only allowed in a loop");
                }
            _ => {}
        }
        RuntimeValue::new_temp(object)
    }

    fn set_return_state(&mut self, state: ReturnState) {
        if self.returned {
            self.die("This scope has already been broken out of");
        }
        self.returned = true;
        self.return_state = state;
    }

    pub fn get_return_state(self) -> ReturnState {
        self.return_state
    }

    #[allow(unreachable_code)]
    fn die(&self, msg: &str) -> RuntimeValue {
        let problem = self.expression_stack.last().unwrap();
        handle_evaluating_error(self.source.as_ref(), problem, msg);
        std::process::exit(0);
        panic!("unreachable")
    }
}