use std::collections::HashMap;

pub type BVId = i32;
pub type BvSymbolsMap = HashMap<BVId, String>;

#[derive(Clone)]
pub enum RecordedOperation {
    Read(String, String), // Target, Value
    Write(String, String), // Target, Value
    Call(String, Vec<String>), // Function name, Vec<Arguments as Strings>
}