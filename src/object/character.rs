use crate::object::*;

pub struct CharObject { 
    val: char 
}

impl ToString for CharObject {
    fn to_string(&self) -> String {
        self.val.to_string()
    }
}

impl Object for CharObject {
    fn display(&self) -> String {
        format!("'{}'", self.val)
    }

    fn get_type(&self) -> TypeObject {
        TypeObject::Char
    }

    fn copy_object(&self) -> ObjectPtr {
        Box::new(Self::new(self.val))
    }

    fn get_char(&self) -> Option<char> {
        Some(self.val)
    }

    fn is_equal(&self, rhs: &ObjectPtr) -> bool {
        let rhs_val = rhs.get_char();
        rhs_val.is_some() && rhs_val.unwrap() == self.val
    }

    fn convert_to_char(&self) -> ObjectRes { Ok(self.copy_object()) }
    fn convert_to_int(&self)  -> ObjectRes { Ok(Box::new(IntObject::new(self.val as i64))) }
    fn convert_to_bool(&self) -> ObjectRes { Ok(Box::new(BoolObject::new(self.val != '\0'))) }
}

impl CharObject {
    pub fn new(c: char) -> Self { Self { val: c } }
}
