use crate::object::*;

pub struct NothingObject {
    reason: Option<String> // Reason this value became Nothing
}

impl ToString for NothingObject {
    fn to_string(&self) -> String {
        match &self.reason {
            None    => "nothing".to_string(),
            Some(s) => format!("nothing (because {})", s)
        }
    }
}

impl Object for NothingObject {
    fn get_type(&self) -> TypeObject {
        TypeObject::NothingType
    }

    fn copy_object(&self) -> ObjectPtr {
        Box::new( Self { reason: self.reason.clone() } )
    }

    fn is_equal(&self, rhs: &ObjectPtr) -> bool {
        rhs.get_type() == TypeObject::NothingType
    }
}

impl NothingObject {
    pub fn new()                    -> Self { Self { reason: None }}
    pub fn new_with_reason(s: &str) -> Self { Self { reason: Some(s.to_string()) }}
}
