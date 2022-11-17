use crate::object::*;

#[derive(Clone, PartialEq)]
pub enum TypeObject {
    Str,
    Int,
    Float,
    Char,
    Bool,
    List,
    Function,
    NothingType,
    TypeID
}

impl ToString for TypeObject {
    fn to_string(&self) -> String {
        (match self {
            Self::Str         => "string",
            Self::Int         => "int",
            Self::Float       => "float",
            Self::Char        => "char",
            Self::Bool        => "bool",
            Self::List        => "list",
            Self::Function    => "function",
            Self::NothingType => "nothingType",
            Self::TypeID      => "typeID"
        }).to_string()
    }
}

impl Object for TypeObject {
    fn get_type(&self) -> TypeObject {
        TypeObject::TypeID
    }

    fn copy_object(&self) -> ObjectPtr {
        Box::new(self.clone())
    }

    fn is_equal(&self, rhs: &ObjectPtr) -> bool {
        let rhs_val = rhs.get_type_variant();
        rhs_val.is_some() && &rhs_val.unwrap() == self
    }

    fn call(&self, args: &Vec<ObjectPtr>) -> ObjectRes {
        if args.len() != 1 {
            return Err("Expected 1 argument for type conversion".to_string());
        }
        let arg = &args[0];
        match self {
            Self::Int    => arg.convert_to_int(),
            Self::Float  => arg.convert_to_float(),
            Self::Char   => arg.convert_to_char(),
            Self::Bool   => arg.convert_to_bool(),
            Self::List   => arg.convert_to_list(),
            Self::Str    => Ok(Box::new(StringObject::new(&arg.to_string()))),
            Self::TypeID => Ok(Box::new(arg.get_type())),
            Self::Function | Self::NothingType => Err(format!(
                "Cannot convert values to type {}", self.to_string()))
        }
    }
}