use crate::backend::{Backend, BV};
use crate::State;
use itertools::Itertools;
use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;

pub type BVId = i32;
pub type BvSymbolsMap = HashMap<BVId, RecordedValue>;


#[derive(Clone)]
pub enum RecordedValue {
    String(String),
    Unknown(String),
    Apply(Box<RecordedValue>, String),
    Constant(String),
    Global(String),
    Deref(Box<RecordedValue>),
    FieldAccess(Box<RecordedValue>, String, String, Vec<Box<RecordedValue>>), // structure base, LLVM structure type string, field name, indices vector (offset)
    BaseArgument(i32, String, String), // parameter ID, name, type
    FunctionReturnValue(Box<RecordedValue>), // Called function (value is not stored)
    FunctionReturnTarget(Box<RecordedValue>), // Called function (value is not stored)
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
            RecordedValue::String(s) => write!(f, "{s}"),
            RecordedValue::Unknown(s) => write!(f, "[unknown: {s}]"),
            RecordedValue::Apply(left, right) =>
                write!(f, "{left} {right}"),
            RecordedValue::Constant(value) => {write!(f, "[constant: {value}]")},
            RecordedValue::Global(value) => {write!(f, "[global: {value}]")},
            RecordedValue::Deref(value) => { write!(f, "{value} deref()") },
            RecordedValue::FieldAccess(struct_base, struct_type, field, offset) => {
                let offsets = offset.iter()
                    .map(|x| { x.to_string() })
                    .join(", ");
                write!(f, "{struct_base} getelementptr(OFFSETS=[{offsets}], FIELD={field}")
            }
            RecordedValue::BaseArgument(param_id, name, arg_type) => {
                write!(f, "base_arg({param_id}, {name}, {arg_type})")
            }
            RecordedValue::FunctionReturnValue(func_name) => {
                write!(f, "func_retval({func_name})")
            }
            RecordedValue::FunctionReturnTarget(func_name) => {
                write!(f, "func_retdest({func_name})")
            }
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


pub fn get_bv_symbol_or_unknown<B: Backend>(state: &State<B>, bv: &<B as Backend>::BV, unknown_str: &str) -> RecordedValue
{
    match state.bv_symbols_map.get(&bv.get_id()) {
        None => { RecordedValue::Unknown(unknown_str.to_string()) }
        Some(x) => { x.clone() }
    }
}

pub fn hasNoUnknownOrFunc(val: &RecordedValue) -> bool {
    match val{
        RecordedValue::String(_) => {true}
        RecordedValue::Unknown(_) => {false}
        RecordedValue::Apply(x, _) => {hasNoUnknownOrFunc(x)}
        RecordedValue::Constant(_) => {true}
        RecordedValue::Global(_) => {true}
        RecordedValue::Deref(x) => {hasNoUnknownOrFunc(x)}
        RecordedValue::FieldAccess(a, b, c, d) => {
            hasNoUnknownOrFunc(a) && d.iter().all(|x| hasNoUnknownOrFunc(x.deref()))
        }
        RecordedValue::BaseArgument(_, _, _) => {true}
        RecordedValue::FunctionReturnValue(_) => {false}
        RecordedValue::FunctionReturnTarget(_) => {false}
    }
}


pub fn comesFromBaseArgument(val: &RecordedValue) -> bool {
    match val{
        RecordedValue::String(_) => {false}
        RecordedValue::Unknown(_) => {false}
        RecordedValue::Apply(x, _) => {comesFromBaseArgument(x)}
        RecordedValue::Constant(_) => {false}
        RecordedValue::Global(_) => {false}
        RecordedValue::Deref(x) => {comesFromBaseArgument(x)}
        RecordedValue::FieldAccess(a, b, c, d) => {
            comesFromBaseArgument(a) || d.iter().all(|x| comesFromBaseArgument(x.deref()))
        }
        RecordedValue::BaseArgument(_, _, _) => {true}
        RecordedValue::FunctionReturnValue(_) => {false}
        RecordedValue::FunctionReturnTarget(_) => {false}
    }
}

