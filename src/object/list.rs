use crate::object::*;
use crate::object::ObjectPtr;

pub struct ListObject { 
    val: Vec<ObjectSharedPtr> 
}

impl ToString for ListObject {
    fn to_string(&self) -> String {
        format!("[{}]", self.val
            .iter()
            .map(|x| x.borrow().display())
            .collect::<Vec<String>>()
            .join(", "))
    }
}

impl Object for ListObject {
    fn get_type(&self) -> TypeObject {
        TypeObject::List
    }

    fn get_contents(&self) -> Option<&Vec<ObjectSharedPtr>> {
        Some(&self.val)
    }

    fn get_length(&self) -> Option<usize> {
        Some(self.val.len())
    }

    fn copy_object(&self) -> ObjectPtr {
        let new_list = self.val
            .iter()
            .map(|x| x.borrow().copy_object())
            .collect::<Vec<ObjectPtr>>();
        Box::new(Self::new(new_list))
    }
    
    fn is_equal(&self, rhs: &ObjectPtr) -> bool {
        if rhs.get_type() != TypeObject::List {
            return false;
        }

        let val = rhs.get_contents().unwrap();

        val.len() == self.val.len()
        && self.val
            .iter()
            .zip(val)
            .all(|(a, b)| a
                .borrow()
                .is_equal(&b.borrow()))
    }

    fn operator_add(&self, rhs: &ObjectPtr) -> ObjectRes {
        if rhs.get_type() != TypeObject::List {
            self.invalid_op(rhs)
        } else {
            let mut res = Vec::<ObjectPtr>::new();
            for o in &self.val {
                res.push(o.borrow().copy_object());
            }
            for o in rhs.get_contents().unwrap() {
                res.push(o.borrow().copy_object());
            }
            let obj = ListObject::new(res);
            Ok(Box::new(obj))
        }
    }

    fn operator_mul(&self, rhs: &ObjectPtr) -> ObjectRes {
        if rhs.get_type() != TypeObject::Int {
            self.invalid_op(rhs)
        } else {
            let int = rhs.get_int().unwrap();
            if int < 0 {
                return Err(format!("Cannot multiply a list {} times", int));
            }
            let m: usize = int.try_into().unwrap();
            if m == 0 {
                return Ok(Box::new(ListObject::new(Vec::new())))
            }
            let mut res = self.copy_object();
            let     obj = self.copy_object();
            for _ in 0 .. m-1 {
                res = res.operator_add(&obj)?;
            }
            Ok(res)
        }
    }

    fn index(&self, index: &ObjectPtr) -> Result<RuntimeValue, String> {
        let i = extract_index(index, self.val.len(), "list")?;
        let res = Rc::clone(&self.val[i]);
        let val = RuntimeValue::new_perm_from_ptr(res);
        Ok(val)
    }
}

impl ListObject {
    pub fn new(l: Vec<ObjectPtr>) -> Self { 
        let mut vals = Vec::<ObjectSharedPtr>::new();
        for o in l {
            vals.push(Rc::new(RefCell::new(o)));
        }
        Self { val: vals } 
    }
}
