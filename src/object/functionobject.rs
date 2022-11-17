use crate::object::*;
use std::rc::Rc;

pub struct FunctionObject { 
    val: Rc<dyn Function> 
}

impl ToString for FunctionObject {
    fn to_string(&self) -> String {
        let raw = Box::into_raw(Box::new(&self.val));
        format!("<Function object at {:X}>", raw as usize)
    }
}

impl Object for FunctionObject {
    fn get_type(&self) -> TypeObject {
        TypeObject::Function
    }

    fn copy_object(&self) -> ObjectPtr {
        Box::new(Self::new(Rc::clone(&self.val)))
    }

    fn is_equal(&self, _rhs: &ObjectPtr) -> bool {
        false
    }

    fn call(&self, args: &Vec<ObjectPtr>) -> ObjectRes {
        self.val.call(args)
    }
}

impl FunctionObject {
    pub fn new(f: Rc<dyn Function>) -> Self { Self { val: f } }
}
