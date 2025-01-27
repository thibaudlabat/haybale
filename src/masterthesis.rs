use std::collections::HashMap;
use std::fmt;

pub type BVId = i32;
pub type BvSymbolsMap = HashMap<BVId, RecordedValue>;


#[derive(Clone)]
pub enum RecordedValue {
    String(String),
    Unknown(String),
    Apply(Box<RecordedValue>, String)
}

#[derive(Clone)]
pub enum RecordedOperation {
    Read(RecordedValue, RecordedValue), // Target, Value
    Write(RecordedValue, RecordedValue), // Target, Value
    Call(RecordedValue, Vec<RecordedValue>) // Function name, Vec<Arguments as Strings>
}

// Implement Display for RecordedOperationValue
impl fmt::Display for RecordedValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RecordedValue::String(s) => write!(f, "{}", s),
            RecordedValue::Unknown(s) => write!(f, "[unknown: {}]", s),
            RecordedValue::Apply(left, right) =>
                write!(f, "{} {}", left, right),
        }
        }
    }


// Implement Display for RecordedOperation
impl fmt::Display for RecordedOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RecordedOperation::Read(target, value) =>
                write!(f, "READ:\n\tTARGET = {}\n\tVALUE = {}", target, value),
            RecordedOperation::Write(target, value) =>
                write!(f, "WRITE:\n\tTARGET = {}\n\tVALUE = {}", target, value),
            RecordedOperation::Call(func_name, args) => {
                // Convert args to strings and join them
                write!(f,"CALL:\n\tFUNCTION = {}\n", func_name);
                for i in 0..args.len() {
                    let arg = &args[i];
                    write!(f,"\targ[{}] = {}\n", i, arg);
                }
                Ok(())
            }
        }
    }
}