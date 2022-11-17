//
// object.rs
// Implements the object system
//
use std::convert::TryInto;
use std::rc::Rc;
use std::cell::RefCell;
use core::fmt::{Formatter, Error};
use crate::function::Function;

pub mod common;
pub mod string;
pub mod int;
pub mod float;
pub mod character;
pub mod boolean;
pub mod list;
pub mod functionobject;
pub mod nothing;
pub mod typeobject;

pub use common::*;
pub use string::*;
pub use int::*;
pub use float::*;
pub use character::*;
pub use boolean::*;
pub use list::*;
pub use functionobject::*;
pub use nothing::*;
pub use typeobject::*;

pub type ObjectPtr = Box<dyn Object>;

pub type ObjectSharedPtr = Rc<RefCell<ObjectPtr>>;

pub struct RuntimeValue {
    pub object: ObjectSharedPtr,
    pub temporary: bool
}

impl RuntimeValue {
    pub fn new(o: ObjectPtr, t: bool) -> Self { Self {
        object: Rc::new(RefCell::new(o)), 
        temporary: t
    }}
    pub fn new_temp(o: ObjectPtr) -> Self { Self::new(o, true)  }
    pub fn new_perm(o: ObjectPtr) -> Self { Self::new(o, false) }

    pub fn new_temp_from_ptr(o: ObjectSharedPtr) -> Self { Self { 
        object: o, temporary: true 
    }}

    pub fn new_perm_from_ptr(o: ObjectSharedPtr) -> Self { Self { 
        object: o, temporary: false 
    }}
}