//
// common.rs
// Contains the object interface, as well 
// as helper functions for objects
//
use crate::object::*;

pub type ObjectRes = Result<ObjectPtr, String>;

//
// Helper functions
//
pub fn extract_index(
    index: &ObjectPtr, max: usize, typename: &str
) -> Result<usize, String> {
    let int = index.get_int();
    if int.is_none() {
        return Err(format!("List indices must be int, not {}", index.typename()))
    }
    let i = int.unwrap() as usize;
    if i >= max {
        return Err(format!(
            "Index {} is out of range for {} of length {}", 
            int.unwrap(), typename, max));
    }
    Ok(i)
}

//
// Object trait
//
pub trait Object: ToString {
    fn display(&self) -> String {
        self.to_string()
    }
    
    fn typename(&self) -> String {
        self.get_type().to_string()
    }

    fn copy_object(&self) -> ObjectPtr;
    
    fn get_type(&self)          -> TypeObject;
    fn get_int(&self)           -> Option<i64>                   { None }
    fn get_float(&self)         -> Option<f64>                   { None }
    fn get_bool(&self)          -> Option<bool>                  { None }
    fn get_char(&self)          -> Option<char>                  { None }
    fn get_type_variant(&self)  -> Option<TypeObject>            { None }
    fn get_contents(&self)      -> Option<&Vec<ObjectSharedPtr>> { None }
    fn get_length(&self)        -> Option<usize>                 { None }

    fn is_equal(&self, rhs: &ObjectPtr) -> bool;

    // 
    // Conversion
    //
    fn convert_error(&self, target: &str) -> ObjectRes {
        Err(format!("Type {} cannot convert to {}", self.typename(), target))
    }

    fn convert_to_int   (&self) -> ObjectRes { self.convert_error("int")    }
    fn convert_to_float (&self) -> ObjectRes { self.convert_error("float")  }
    fn convert_to_char  (&self) -> ObjectRes { self.convert_error("char")   }
    fn convert_to_bool  (&self) -> ObjectRes { self.convert_error("bool")   }
    fn convert_to_list  (&self) -> ObjectRes { self.convert_error("list")   }
    
    //
    // Operators
    //
    fn invalid_op(&self, rhs: &ObjectPtr) -> ObjectRes {
        Err(format!(
            "This operator is not valid for types {} and {}",
            self.typename(), 
            rhs.typename()))
    }

    fn invalid_uop(&self) -> ObjectRes {
        Err(format!("This operator is not valid for type {}", self.typename()))
    }
    // Arithmetic
    fn operator_add (&self, rhs: &ObjectPtr) -> ObjectRes { self.invalid_op(rhs) }
    fn operator_sub (&self, rhs: &ObjectPtr) -> ObjectRes { self.invalid_op(rhs) }
    fn operator_mul (&self, rhs: &ObjectPtr) -> ObjectRes { self.invalid_op(rhs) }
    fn operator_div (&self, rhs: &ObjectPtr) -> ObjectRes { self.invalid_op(rhs) }
    fn operator_mod (&self, rhs: &ObjectPtr) -> ObjectRes { self.invalid_op(rhs) }
    fn operator_neg (&self)                  -> ObjectRes { self.invalid_uop()   }
    fn operator_pos (&self)                  -> ObjectRes { self.invalid_uop()   }

    // Logical
    fn operator_or  (&self, rhs: &ObjectPtr) -> ObjectRes { self.invalid_op(rhs) }
    fn operator_and (&self, rhs: &ObjectPtr) -> ObjectRes { self.invalid_op(rhs) }
    fn operator_not (&self)                  -> ObjectRes { self.invalid_uop()   }

    // Comparison
    fn operator_eq(&self, rhs: &ObjectPtr) -> ObjectRes { 
        Ok(Box::new(BoolObject::new(self.is_equal(rhs))))
    }
    fn operator_neq(&self, rhs: &ObjectPtr) -> ObjectRes { 
        Ok(Box::new(BoolObject::new(!self.is_equal(rhs))))
    }
    fn operator_lt    (&self, rhs: &ObjectPtr) -> ObjectRes { self.invalid_op(rhs) }
    fn operator_gt    (&self, rhs: &ObjectPtr) -> ObjectRes { self.invalid_op(rhs) }
    fn operator_lt_eq (&self, rhs: &ObjectPtr) -> ObjectRes { self.invalid_op(rhs) }
    fn operator_gt_eq (&self, rhs: &ObjectPtr) -> ObjectRes { self.invalid_op(rhs) }

    // Bitwise
    fn operator_bw_or     (&self, rhs: &ObjectPtr) -> ObjectRes { self.invalid_op(rhs) }
    fn operator_bw_and    (&self, rhs: &ObjectPtr) -> ObjectRes { self.invalid_op(rhs) }
    fn operator_bw_xor    (&self, rhs: &ObjectPtr) -> ObjectRes { self.invalid_op(rhs) }
    fn operator_bw_not    (&self)                     -> ObjectRes { self.invalid_uop()   }
    fn operator_bw_shiftl (&self, rhs: &ObjectPtr) -> ObjectRes { self.invalid_op(rhs) }
    fn operator_bw_shiftr (&self, rhs: &ObjectPtr) -> ObjectRes { self.invalid_op(rhs) }

    //
    // Other operations
    //
    fn call(&self, _args: &Vec<ObjectPtr>) -> ObjectRes {
        Err(format!("type {} is not callable", self.typename()))
    }

    fn index(&self, _index: &ObjectPtr) -> Result<RuntimeValue, String> {
        Err(format!("type {} cannot be indexed", self.typename()))
    }
}

impl std::fmt::Debug for dyn Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        f.write_str(&self.to_string())
    }
}