use crate::object::*;

pub struct FloatObject { 
    val: f64 
}

impl ToString for FloatObject {
    fn to_string(&self) -> String {
        format!("{}f", self.val)//self.val.to_string()
    }
}

impl Object for FloatObject {
    fn get_type(&self) -> TypeObject {
        TypeObject::Float
    }
    
    fn copy_object(&self) -> ObjectPtr {
        Box::new(Self::new(self.val))
    }
    
    fn get_float(&self) -> Option<f64> {
        Some(self.val)
    }

    fn is_equal(&self, rhs: &ObjectPtr) -> bool {
        let rhs_val = rhs.get_float();
        rhs_val.is_some() && rhs_val.unwrap() == self.val
    }

    /*
     * Operators
     */
    // Arithmetic
    fn operator_add(&self, rhs: &ObjectPtr) -> ObjectRes {
        self.do_op(rhs, |x, y| FloatObject::new(x + y))
    }

    fn operator_sub(&self, rhs: &ObjectPtr) -> ObjectRes {
        self.do_op(rhs, |x, y| FloatObject::new(x - y))
    }

    fn operator_mul(&self, rhs: &ObjectPtr) -> ObjectRes {
        self.do_op(rhs, |x, y| FloatObject::new(x * y))
    }

    fn operator_div(&self, rhs: &ObjectPtr) -> ObjectRes {
        self.do_op(rhs, |x, y| FloatObject::new(x / y))
    }

    fn operator_mod(&self, rhs: &ObjectPtr) -> ObjectRes {
        self.do_op(rhs, |x, y| FloatObject::new(x % y))
    }

    // Comparison
    fn operator_lt(&self, rhs: &ObjectPtr) -> ObjectRes { 
        self.do_op(rhs, |x, y| BoolObject::new(x < y))
    }

    fn operator_gt(&self, rhs: &ObjectPtr) -> ObjectRes { 
        self.do_op(rhs, |x, y| BoolObject::new(x > y))
    }

    fn operator_lt_eq(&self, rhs: &ObjectPtr) -> ObjectRes { 
        self.do_op(rhs, |x, y| BoolObject::new(x <= y))
    }

    fn operator_gt_eq(&self, rhs: &ObjectPtr) -> ObjectRes { 
        self.do_op(rhs, |x, y| BoolObject::new(x >= y))
    }

    fn operator_neg(&self) -> ObjectRes {
        Ok(Box::new(FloatObject::new(-self.val)))
    }

    fn operator_pos(&self) -> ObjectRes {
        Ok(self.copy_object())
    }

    /*
     * Conversion
     */
    fn convert_to_float(&self) -> ObjectRes { Ok(self.copy_object())  }
    fn convert_to_int(&self)   -> ObjectRes { Ok(Box::new(IntObject::new(self.val as i64)))  }
    fn convert_to_bool(&self)  -> ObjectRes { Ok(Box::new(BoolObject::new(self.val != 0.0))) }
}

impl FloatObject {
    pub fn new(f: f64) -> Self { Self { val: f } }

    pub fn do_op<T: Object + 'static>(
        &self, 
        rhs: &ObjectPtr,
        op: fn(f64, f64) -> T
    ) -> ObjectRes {
        let rhs_val = rhs.get_float();
        match rhs_val {
            Some(v) => {
                let res = op(self.val, v);
                Ok(Box::new(res))
            },
            None => self.invalid_op(rhs)
        }
    }
}
