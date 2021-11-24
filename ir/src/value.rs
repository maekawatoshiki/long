use id_arena::{Arena, Id};

pub type ValueId = Id<Value>;

/// A value in the IR.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(IntValue),
}

/// An arena for `Value`.
#[derive(Debug, Clone)]
pub struct ValueArena(Arena<Value>);

/// An integer value in the IR.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IntValue {
    // TODO: Add more variants.
    Int32(i32),
}

impl ValueArena {
    /// Creates a new `ValueArena`.
    ///
    /// # Examples
    ///
    /// ```
    /// use long_ir::value::ValueArena;
    /// ValueArena::new();
    /// ```
    pub fn new() -> Self {
        ValueArena(Arena::new())
    }

    /// Creates a new `ValueId` for the given `val`.
    ///
    /// # Examples
    ///
    /// ```
    /// use long_ir::value::ValueArena;
    /// let mut arena = ValueArena::new();
    /// let _id = arena.new_int(42i32);
    /// ```
    pub fn new_int<T>(&mut self, val: T) -> ValueId
    where
        T: Into<IntValue>,
    {
        self.0.alloc(val.into().into())
    }

    /// Returns the reference to the value if exists.
    ///
    /// # Examples
    ///
    /// ```
    /// use long_ir::value::{ValueArena, IntValue};
    /// let mut arena = ValueArena::new();
    /// let id = arena.new_int(42i32);
    /// assert_eq!(arena.get(id).unwrap(), &IntValue::Int32(42).into());
    /// ```
    pub fn get(&self, id: ValueId) -> Option<&Value> {
        self.0.get(id)
    }

    /// Returns the mutable reference to the value if exists.
    ///
    /// # Examples
    ///
    /// ```
    /// use long_ir::value::{ValueArena, IntValue};
    /// let mut arena = ValueArena::new();
    /// let id = arena.new_int(42i32);
    /// assert_eq!(arena.get_mut(id).unwrap(), &mut IntValue::Int32(42).into());
    /// ```
    pub fn get_mut(&mut self, id: ValueId) -> Option<&mut Value> {
        self.0.get_mut(id)
    }
}

impl From<IntValue> for Value {
    fn from(int: IntValue) -> Self {
        Value::Int(int)
    }
}

impl From<i32> for IntValue {
    fn from(int: i32) -> Self {
        IntValue::Int32(int)
    }
}
