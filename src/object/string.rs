use crate::object::*;

pub struct StringObject { 
    val: String 
}

impl ToString for StringObject {
    fn to_string(&self) -> String {
        self.val.clone()
    }
}

impl Object for StringObject {
    fn display(&self) -> String {
        format!("\"{}\"", self.val)
    }

    fn get_type(&self) -> TypeObject {
        TypeObject::Str
    }

    fn get_length(&self) -> Option<usize> {
        Some(self.val.len())
    }

    fn copy_object(&self) -> ObjectPtr {
        Box::new(Self::new(&self.val))
    }

    fn is_equal(&self, rhs: &ObjectPtr) -> bool {
        rhs.get_type()    == TypeObject::Str 
        && 
        self.to_string() == rhs.to_string()
    }

    fn operator_add(&self, rhs: &ObjectPtr) -> ObjectRes {
        if rhs.get_type() != TypeObject::Str {
            self.invalid_op(rhs)
        } else {
            let string = format!("{}{}", &self.val, rhs.to_string());
            let obj = StringObject::new(&string);
            Ok(Box::new(obj))
        }
    }

    fn operator_mul(&self, rhs: &ObjectPtr) -> ObjectRes {
        if rhs.get_type() != TypeObject::Int {
            self.invalid_op(rhs)
        } else {
            let int = rhs.get_int().unwrap();
            if int < 0 {
                return Err(format!("Cannot multiply a string {} times", int));
            }
            let i: usize = int.try_into().unwrap();
            let obj = StringObject::new(&self.val.repeat(i));
            Ok(Box::new(obj))
        }
    }

    fn convert_to_int(&self) -> ObjectRes { 
        let val = self.val.parse::<i64>();
        match val {
            Ok(v)  => Ok(Box::new(IntObject::new(v))),
            Err(_) => self.string_convert_error("int")
        }
    }

    fn convert_to_float(&self) -> ObjectRes { 
        let val = self.val.parse::<f64>();
        match val {
            Ok(v)  => Ok(Box::new(FloatObject::new(v))),
            Err(_) => self.string_convert_error("float")
        }
    }

    fn convert_to_char(&self) -> ObjectRes { 
        if self.val.len() != 1 {
            return Err(format!(
                "Strings must have a length of 1 to convert to char: {}", self.val));
        }
        let val = self.val.chars().nth(0).unwrap();
        Ok(Box::new(CharObject::new(val)))
    }

    fn convert_to_bool(&self) -> ObjectRes { 
        Ok(Box::new(BoolObject::new(
            if self.val == "true" {
                true
            } else if self.val == "false" {
                false
            } else {
                return self.string_convert_error("bool");
            }
        )))
    }

    fn convert_to_list(&self) -> ObjectRes { 
        let mut chars = Vec::<ObjectPtr>::new();
        for c in self.val.chars() {
            chars.push(Box::new(CharObject::new(c)));
        }
        Ok(Box::new(ListObject::new(chars)))
    }

    fn index(&self, index: &ObjectPtr) -> Result<RuntimeValue, String> {
        let i = extract_index(index, self.val.len(), "list")?;
        let c = self.val.chars().nth(i).unwrap();
        let res = Box::new(CharObject::new(c));
        let val = RuntimeValue::new_temp(res);
        Ok(val)
    }
}

impl StringObject {
    pub fn new(s: &str) -> Self { Self { val: s.to_string()} }

    fn string_convert_error(&self, target: &str) -> ObjectRes {
        Err(format!("string \"{}\" cannot convert to {}", self.val, target))
    }
}
