use crate::backend::{Backend, BV};
use crate::State;
use itertools::Itertools;
use llvm_ir::instruction::groups::BinaryOp;
use llvm_ir::Operand;
use std::collections::HashMap;
use std::fmt;

pub type BVId = i32;
pub type BvSymbolsMap = HashMap<BVId, RecordedValue>;
use serde::{Deserialize, Serialize};


#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
pub enum RecordedValue {
    DebugString(String),
    Unknown(String),
    Apply(Box<RecordedValue>, String),
    Constant(String),
    Global(String),
    Deref(Box<RecordedValue>),
    FieldAccess(Box<RecordedValue>, String, String, Vec<Box<RecordedValue>>), // structure base, LLVM structure type string, field name, indices vector (offset)
    BaseArgument(i32, String, String), // parameter ID, name, type
    BinaryOperation(Box<RecordedValue>, Box<RecordedValue>, String),

    ICmp(Box<RecordedValue>, Box<RecordedValue>, String), // a, b, predicate as a string
    Function(String),
    FunctionPointer(Box<RecordedValue>),
    UnevaluatedFunctionReturnValue(String), // function name
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
pub enum RecordedOperation {
    Read(RecordedValue, RecordedValue), // Target, Value
    Write(RecordedValue, RecordedValue), // Target, Value
    Call(RecordedValue, Vec<RecordedValue>, bool), // Function name, Vec<Arguments as Strings>, isFound
    CondBranch(RecordedValue, bool),
}

// Implement Display for RecordedOperationValue
impl fmt::Display for RecordedValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RecordedValue::DebugString(s) => write!(f, "{s}"),
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
            RecordedValue::BinaryOperation(a, b, binop) => {
                write!(f, "binop({}, {}, {:?})", a, b, binop)
            }
            RecordedValue::ICmp(a, b, predicate) => {
                write!(f, "icmp({}, {}, {:?})", a, b, predicate)
            }
            RecordedValue::FunctionPointer(func) => {
                write!(f, "funcptr({func})")
            }
            RecordedValue::Function(func) => {
                write!(f, "func({func})")
            }
            RecordedValue::UnevaluatedFunctionReturnValue(func) => {
                write!(f, "uneval_retval({func})")
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
            RecordedOperation::Call(func_name, args, isFound) => {
                // Convert args to strings and join them
                write!(f,"CALL:\n\tFUNCTION = {}\n\tFOUND = {}\n", func_name, isFound);
                for i in 0..args.len() {
                    let arg = &args[i];
                    write!(f,"\targ[{}] = {}\n", i, arg);
                }
                Ok(())
            }
            RecordedOperation::CondBranch(a, isTrue) => {
                write!(f, "CondBranch:\n\tCONDITION = {}\n\tisTrue = {}", a, isTrue)
            }
        }
    }
}


pub fn get_bv_symbol_or_unknown<B: Backend>(state: &State<B>, bv: &<B as Backend>::BV, unknown_str: &str) -> RecordedValue
{
    match state.trace.bv_symbols_map.get(&bv.get_id()) {
        None => { RecordedValue::Unknown(unknown_str.to_string()) }
        Some(x) => { x.clone() }
    }
}

pub fn get_operand_symbol_or_unknown<B: Backend>(state: &State<B>, op: &Operand, unknown_str: &str) -> RecordedValue
{
    match op{
        Operand::LocalOperand { .. } => {
            let bv = state.operand_to_bv(&op).unwrap();
            match state.trace.bv_symbols_map.get(&bv.get_id()) {
                None => { RecordedValue::Unknown(unknown_str.to_string()) }
                Some(x) => { x.clone() }
            }
        }
        Operand::ConstantOperand(const_op) => {
            RecordedValue::Constant(const_op.to_string())
        }
        Operand::MetadataOperand => {
            panic!("metadata operand");
        }
    }
}



pub fn binaryOpToString(bop: &BinaryOp) -> String{
    match bop{
        BinaryOp::Add(_) => {"Add"}
        BinaryOp::Sub(_) => {"Sub"}
        BinaryOp::Mul(_) => {"Mul"}
        BinaryOp::UDiv(_) => {"UDiv"}
        BinaryOp::SDiv(_) => {"SDiv"}
        BinaryOp::URem(_) => {"URem"}
        BinaryOp::SRem(_) => {"SRem"}
        BinaryOp::And(_) => {"And"}
        BinaryOp::Or(_) => {"Or"}
        BinaryOp::Xor(_) => {"Xor"}
        BinaryOp::Shl(_) => {"Shl"}
        BinaryOp::LShr(_) => {"LShr"}
        BinaryOp::AShr(_) => {"AShr"}
        BinaryOp::FAdd(_) => {"FAdd"}
        BinaryOp::FSub(_) => {"FSub"}
        BinaryOp::FMul(_) => {"FMul"}
        BinaryOp::FDiv(_) => {"FDiv"}
        BinaryOp::FRem(_) => {"FRem"}
    }.to_string()
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct FunctionTrace{
    pub bv_symbols_map : BvSymbolsMap,
    pub recorded_operations: Vec<RecordedOperation>,
    pub instrCount: u32,
    pub hasUnresolvedFunctions: bool
}

impl Default for FunctionTrace {
    fn default() -> FunctionTrace {
        FunctionTrace {
            bv_symbols_map: Default::default(),
            recorded_operations: vec![],
            instrCount: 0,
            hasUnresolvedFunctions: false,
        }
    }
}