use crate::object::*;

pub struct BoolObject { 
    val: bool 
}

impl ToString for BoolObject {
    fn to_string(&self) -> String {
        self.val.to_string()
    }
}

impl Object for BoolObject {
    fn get_type(&self) -> TypeObject {
        TypeObject::Bool
    }

    fn copy_object(&self) -> ObjectPtr {
        Box::new(Self::new(self.val))
    }

    fn get_bool(&self) -> Option<bool> {
        Some(self.val)
    }

    fn is_equal(&self, rhs: &ObjectPtr) -> bool {
        let rhs_val = rhs.get_bool();
        rhs_val.is_some() && rhs_val.unwrap() == self.val
    }

    fn operator_or(&self, rhs: &ObjectPtr) -> ObjectRes {
        let r = rhs.get_bool();
        Ok(match r {
            Some(rval) => Box::new(BoolObject::new(self.val || rval)),
            None       => Box::new(
                NothingObject::new_with_reason("bool or'ed with non bool"))
        })
    }

    fn operator_and(&self, rhs: &ObjectPtr) -> ObjectRes {
        let r = rhs.get_bool();
        Ok(match r {
            Some(rval) => Box::new(BoolObject::new(self.val && rval)),
            None       => Box::new(
                NothingObject::new_with_reason("bool and'ed with non bool"))
        })
    }

    fn operator_not(&self) -> ObjectRes {
        Ok(Box::new(BoolObject::new(!self.val)))
    }

    fn convert_to_bool(&self) -> ObjectRes { 
        Ok(self.copy_object())
    }

    fn convert_to_int(&self) -> ObjectRes { 
        let v = if self.val { 1 } else { 0 };
        Ok(Box::new(IntObject::new(v)))
    }
}

impl BoolObject {
    pub fn new(b: bool) -> Self { Self { val: b } }
}
