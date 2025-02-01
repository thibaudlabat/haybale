//! For an introduction to the crate and how to get started,
//! see the [crate's README](https://github.com/PLSysSec/haybale/blob/master/README.md).

// this ensures that crate users generating docs with --no-deps will still
// properly get links to the public docs for haybale's types
// it was especially necessary when the docs.rs docs weren't working for any
// llvm-sys consumers; now that we have docs.rs as the official docs, I'm not
// sure if this is necessary or helpful anymore
#![doc(html_root_url = "https://docs.rs/haybale/")]

use llvm_ir::Type;
use std::collections::HashSet;

mod project;
pub use project::Project;

mod symex;
pub use symex::*;

pub mod config;
pub use config::Config;

mod error;
pub use error::*;

mod parameter_val;
pub use parameter_val::ParameterVal;

mod return_value;
pub use return_value::ReturnValue;

mod alloc;
pub mod alloc_utils;
pub mod backend;
pub mod callbacks;
pub mod cell_memory;
mod demangling;
mod double_keyed_map;
pub mod function_hooks;
mod global_allocations;
pub mod hook_utils;
mod hooks;
pub mod solver_utils;
mod state;
pub use state::get_path_length;
mod varmap;
pub mod watchpoints;

use backend::*;
use itertools::Itertools;
use solver_utils::PossibleSolutions;

#[cfg(test)]
mod test_utils;
mod masterthesis;

/// A simple enum describing either an integer value or a pointer
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum SolutionValue {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    Ptr(u64),
}

impl SolutionValue {
    pub fn unwrap_to_i8(self) -> i8 {
        match self {
            SolutionValue::I8(i) => i,
            _ => panic!("unwrap_to_i8 on {:?}", self),
        }
    }

    pub fn unwrap_to_i16(self) -> i16 {
        match self {
            SolutionValue::I16(i) => i,
            _ => panic!("unwrap_to_i16 on {:?}", self),
        }
    }

    pub fn unwrap_to_i32(self) -> i32 {
        match self {
            SolutionValue::I32(i) => i,
            _ => panic!("unwrap_to_i32 on {:?}", self),
        }
    }

    pub fn unwrap_to_i64(self) -> i64 {
        match self {
            SolutionValue::I64(i) => i,
            _ => panic!("unwrap_to_i64 on {:?}", self),
        }
    }

    pub fn unwrap_to_ptr(self) -> u64 {
        match self {
            SolutionValue::Ptr(u) => u,
            _ => panic!("unwrap_to_ptr on {:?}", self),
        }
    }
}

pub use masterthesis::*;