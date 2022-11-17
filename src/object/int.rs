use crate::object::*;

pub struct IntObject { 
    val: i64 
}

impl ToString for IntObject {
    fn to_string(&self) -> String {
        self.val.to_string()
    }
}

impl Object for IntObject {
    fn get_type(&self) -> TypeObject {
        TypeObject::Int
    }

    fn copy_object(&self) -> ObjectPtr {
        Box::new(Self::new(self.val))
    }

    fn get_int(&self) -> Option<i64> {
        Some(self.val)
    }

    fn get_float(&self) -> Option<f64> {
        Some(self.val as f64)
    }

    fn is_equal(&self, rhs: &ObjectPtr) -> bool {
        if rhs.get_type() == TypeObject::Float {
            let lhs = FloatObject::new(self.val as f64);
            return lhs.is_equal(rhs);
        }
        let rhs_val = rhs.get_int();
        rhs_val.is_some() && rhs_val.unwrap() == self.val
    }

    /*
     * Operations
     */ 
    // Arithmetic 
    fn operator_add(&self, rhs: &ObjectPtr) -> ObjectRes {
        self.do_op(rhs, |x, y| IntObject::new(x + y), |x, y| x.operator_add(y))
    }

    fn operator_sub(&self, rhs: &ObjectPtr) -> ObjectRes {
        self.do_op(rhs, |x, y| IntObject::new(x - y), |x, y| x.operator_sub(y))
    }

    fn operator_mul(&self, rhs: &ObjectPtr) -> ObjectRes {
        self.do_op(rhs, |x, y| IntObject::new(x * y), |x, y| x.operator_mul(y))
    }

    fn operator_div(&self, rhs: &ObjectPtr) -> ObjectRes {
        self.do_checked_op(rhs, |x, y| x.checked_div(y), |x, y| x.operator_div(y))
    }

    fn operator_mod(&self, rhs: &ObjectPtr) -> ObjectRes {
        self.do_checked_op(rhs, |x, y| x.checked_rem(y), |x, y| x.operator_mod(y))
    }

    fn operator_neg(&self) -> ObjectRes {
        Ok(Box::new(IntObject::new(-self.val)))
    }

    fn operator_pos(&self) -> ObjectRes {
        Ok(self.copy_object())
    }

    // Comparison
    fn operator_lt(&self, rhs: &ObjectPtr) -> ObjectRes { 
        self.do_op(rhs, |x, y| BoolObject::new(x < y), |x, y| x.operator_lt(y))
    }

    fn operator_gt(&self, rhs: &ObjectPtr) -> ObjectRes { 
        self.do_op(rhs, |x, y| BoolObject::new(x > y), |x, y| x.operator_gt(y))
    }

    fn operator_lt_eq(&self, rhs: &ObjectPtr) -> ObjectRes { 
        self.do_op(rhs, |x, y| BoolObject::new(x <= y), |x, y| x.operator_lt_eq(y))
    }

    fn operator_gt_eq(&self, rhs: &ObjectPtr) -> ObjectRes { 
        self.do_op(rhs, |x, y| BoolObject::new(x >= y), |x, y| x.operator_gt_eq(y))
    }

    // Bitwise
    fn operator_bw_or(&self, rhs: &ObjectPtr) -> ObjectRes { 
        self.do_bitwise_op(rhs, |x, y| x | y)
    }

    fn operator_bw_and(&self, rhs: &ObjectPtr) -> ObjectRes { 
        self.do_bitwise_op(rhs, |x, y| x & y)
    }

    fn operator_bw_xor(&self, rhs: &ObjectPtr) -> ObjectRes { 
        self.do_bitwise_op(rhs, |x, y| x ^ y)
    }

    fn operator_bw_shiftl(&self, rhs: &ObjectPtr) -> ObjectRes { 
        self.do_bitwise_op(rhs, |x, y| x << y) 
    }

    fn operator_bw_shiftr(&self, rhs: &ObjectPtr) -> ObjectRes { 
        self.do_bitwise_op(rhs, |x, y| x >> y)
    }

    fn operator_bw_not(&self) -> ObjectRes {
        Ok(Box::new(IntObject::new(!self.val)))
    }

    /*
     * Conversions
     */
    fn convert_to_int(&self)   -> ObjectRes { Ok(self.copy_object()) }
    fn convert_to_float(&self) -> ObjectRes { Ok(Box::new(FloatObject::new(self.val as f64)))  }
    fn convert_to_bool(&self)  -> ObjectRes { Ok(Box::new(BoolObject::new(self.val != 0)))   }

    fn convert_to_char(&self) -> ObjectRes { 
        if self.val < 0 || self.val > 255 {
            Err(format!("Value {} is outside the 0-255 range for char", self.val))
        } else {
            let asu8: u8 = ((self.val as u64) & 0xFF).try_into().unwrap();
            Ok(Box::new(CharObject::new(asu8 as char)))
        }
    }
}

type FloatOperation = fn(&FloatObject, &ObjectPtr) -> ObjectRes;

impl IntObject {
    pub fn new(i: i64) -> Self { Self { val: i } }

    fn do_op<T: Object + 'static>(
        &self, 
        rhs: &ObjectPtr,
        int_op:   fn(i64, i64) -> T,
        float_op: FloatOperation
    ) -> ObjectRes {
        if rhs.get_type() == TypeObject::Float {
            let lhs = FloatObject::new(self.val as f64);
            return float_op(&lhs, rhs);
        }
        let rhs_val = rhs.get_int();
        match rhs_val {
            Some(v) => {
                let res = int_op(self.val, v);
                Ok(Box::new(res))
            },
            None => self.invalid_op(rhs)
        }
    }

    fn do_checked_op(
        &self,
        rhs: &ObjectPtr,
        int_op: fn(i64, i64) -> Option<i64>,
        float_op: FloatOperation
    ) -> ObjectRes {
        if rhs.get_type() == TypeObject::Float {
            let lhs = FloatObject::new(self.val as f64);
            return float_op(&lhs, rhs);
        }
        let rhs_val = rhs.get_int();
        if rhs_val.is_none() {
            return self.invalid_op(rhs);
        }
        let res = int_op(self.val, rhs_val.unwrap());
        match res {
            Some(v) => Ok(Box::new(IntObject::new(v))),
            None    => Err("Division by zero error".to_string())
        }
    }

    fn do_bitwise_op(
        &self,
        rhs: &ObjectPtr,
        op: fn(i64, i64) -> i64
     ) -> ObjectRes {
        let rhs_val = rhs.get_int();
        match rhs_val {
            Some(v) => {
                let res = op(self.val, v);
                Ok(Box::new(IntObject::new(res)))
            },
            None => self.invalid_op(rhs)
        }
    }
}
