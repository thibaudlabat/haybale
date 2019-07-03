use inkwell::basic_block::BasicBlock;
use inkwell::types::*;
use inkwell::values::*;
use log::debug;
use z3::ast::{Ast, BV, Bool};

use crate::iterators::*;
use crate::state::State;
use crate::utils::*;

// Symex the given function and return the new BV (AST) representing its return value.
// Assumes that the function's parameters are already added to the state.
pub fn symex_function<'ctx>(state: &mut State<'ctx>, func: FunctionValue) -> BV<'ctx> {
    debug!("Symexing function {}", get_func_name(func));
    let bb = func.get_entry_basic_block().expect("Failed to get entry basic block");
    symex_from_bb(state, bb, None)
}

// TODO: Feels hacky, make better
// After calling symex_function() on the State once, caller can then call
// symex_again() to get a different solution (a different State and a
// different returned BV), or None if there are no more possibilities.
pub fn symex_again<'ctx>(state: &mut State<'ctx>) -> Option<BV<'ctx>> {
    if let Some((bb, prev_bb)) = state.revert_to_backtracking_point() {
        Some(symex_from_bb(state, bb, Some(prev_bb)))
    } else {
        None
    }
}

// Symex the given bb, through the rest of the function.
// Returns the new BV representing the return value of the function.
fn symex_from_bb<'ctx>(state: &mut State<'ctx>, bb: BasicBlock, prev_bb: Option<BasicBlock>) -> BV<'ctx> {
    debug!("Symexing basic block {}", get_bb_name(bb));
    let insts = InstructionIterator::new(&bb);
    for inst in insts {
        let opcode = inst.get_opcode();
        if let Some(z3binop) = opcode_to_binop(&opcode) {
            symex_binop(state, inst, z3binop);
        } else if opcode == InstructionOpcode::ICmp {
            symex_icmp(state, inst);
        } else if opcode == InstructionOpcode::Load {
            symex_load(state, inst);
        } else if opcode == InstructionOpcode::Store {
            symex_store(state, inst);
        } else if opcode == InstructionOpcode::GetElementPtr {
            symex_gep(state, inst);
        } else if opcode == InstructionOpcode::Alloca {
            symex_alloca(state, inst);
        } else if opcode == InstructionOpcode::ZExt {
            symex_zext(state, inst);
        } else if opcode == InstructionOpcode::SExt {
            symex_sext(state, inst);
        } else if opcode == InstructionOpcode::Trunc {
            symex_trunc(state, inst);
        } else if opcode == InstructionOpcode::BitCast {
            symex_bitcast(state, inst);
        } else if opcode == InstructionOpcode::Phi {
            symex_phi(state, inst, prev_bb);
        } else if opcode == InstructionOpcode::Select {
            symex_select(state, inst);
        } else if opcode == InstructionOpcode::Call {
            symex_call(state, inst);
        } else if opcode == InstructionOpcode::Return {
            return symex_return(state, inst);
        } else if opcode == InstructionOpcode::Br {
            return symex_br(state, inst, bb);
        } else {
            unimplemented!("instruction {:?}", opcode);
        }
    }
    panic!("No terminator found in function");
}

fn opcode_to_binop<'ctx>(opcode: &InstructionOpcode) -> Option<Box<FnOnce(&BV<'ctx>, &BV<'ctx>) -> BV<'ctx>>> {
    match opcode {
        InstructionOpcode::Add => Some(Box::new(BV::bvadd)),
        InstructionOpcode::Sub => Some(Box::new(BV::bvsub)),
        InstructionOpcode::Mul => Some(Box::new(BV::bvmul)),
        InstructionOpcode::UDiv => Some(Box::new(BV::bvudiv)),
        InstructionOpcode::SDiv => Some(Box::new(BV::bvsdiv)),
        InstructionOpcode::URem => Some(Box::new(BV::bvurem)),
        InstructionOpcode::SRem => Some(Box::new(BV::bvsrem)),
        InstructionOpcode::And => Some(Box::new(BV::bvand)),
        InstructionOpcode::Or => Some(Box::new(BV::bvor)),
        InstructionOpcode::Xor => Some(Box::new(BV::bvxor)),
        InstructionOpcode::Shl => Some(Box::new(BV::bvshl)),
        InstructionOpcode::LShr => Some(Box::new(BV::bvlshr)),
        InstructionOpcode::AShr => Some(Box::new(BV::bvashr)),
        _ => None,
    }
}

fn intpred_to_z3pred<'ctx>(pred: inkwell::IntPredicate) -> Box<FnOnce(&BV<'ctx>, &BV<'ctx>) -> Bool<'ctx>> {
    match pred {
        inkwell::IntPredicate::EQ => Box::new(|a,b| BV::_eq(a,b)),
        inkwell::IntPredicate::NE => Box::new(|a,b| Bool::not(&BV::_eq(a,b))),
        inkwell::IntPredicate::UGT => Box::new(BV::bvugt),
        inkwell::IntPredicate::UGE => Box::new(BV::bvuge),
        inkwell::IntPredicate::ULT => Box::new(BV::bvult),
        inkwell::IntPredicate::ULE => Box::new(BV::bvule),
        inkwell::IntPredicate::SGT => Box::new(BV::bvsgt),
        inkwell::IntPredicate::SGE => Box::new(BV::bvsge),
        inkwell::IntPredicate::SLT => Box::new(BV::bvslt),
        inkwell::IntPredicate::SLE => Box::new(BV::bvsle),
    }
}

fn symex_binop<'ctx, F>(state: &mut State<'ctx>, inst: InstructionValue, z3op: F)
    where F: FnOnce(&BV<'ctx>, &BV<'ctx>) -> BV<'ctx>
{
    debug!("Symexing binop {}", &get_value_name(inst));
    assert_eq!(inst.get_num_operands(), 2);
    let firstop = inst.get_operand(0).unwrap().left().unwrap();
    let secondop = inst.get_operand(1).unwrap().left().unwrap();
    let z3firstop = state.operand_to_bv(firstop);
    let z3secondop = state.operand_to_bv(secondop);
    let width = firstop.get_type().as_int_type().get_bit_width();
    assert_eq!(width, secondop.get_type().as_int_type().get_bit_width());
    let dest = get_dest_name(inst);
    let z3dest = BV::new_const(state.ctx, dest, width);
    state.assert(&z3dest._eq(&z3op(&z3firstop, &z3secondop)));
    state.add_bv_var(inst, z3dest);
}

fn symex_icmp(state: &mut State, inst: InstructionValue) {
    debug!("Symexing icmp {}", &get_value_name(inst));
    assert_eq!(inst.get_num_operands(), 2);
    let dest = get_dest_name(inst);
    let z3dest = Bool::new_const(state.ctx, dest);
    let firstop = inst.get_operand(0).unwrap().left().unwrap();
    let secondop = inst.get_operand(1).unwrap().left().unwrap();
    let z3firstop = state.operand_to_bv(firstop);
    let z3secondop = state.operand_to_bv(secondop);
    let z3pred = intpred_to_z3pred(inst.get_icmp_predicate().unwrap());
    state.assert(&z3dest._eq(&z3pred(&z3firstop, &z3secondop)));
    state.add_bool_var(inst, z3dest);
}

fn symex_zext(state: &mut State, inst: InstructionValue) {
    debug!("Symexing zext {}", &get_value_name(inst));
    debug!("(zext as debug is {:?}", inst);
    assert_eq!(inst.get_num_operands(), 1);
    let op = inst.get_operand(0).unwrap().left().unwrap();
    let z3op = state.operand_to_bv(op);
    let source_size = z3op.get_size();
    let dest_size = get_dest_type(inst).into_int_type().get_bit_width();
    let dest = get_dest_name(inst);
    let z3dest = BV::new_const(state.ctx, dest, dest_size);
    state.assert(&z3dest._eq(&z3op.zero_ext(dest_size - source_size)));
    state.add_bv_var(inst, z3dest);
}

fn symex_sext(state: &mut State, inst: InstructionValue) {
    debug!("Symexing sext {}", &get_value_name(inst));
    debug!("(sext as debug is {:?}", inst);
    assert_eq!(inst.get_num_operands(), 1);
    let op = inst.get_operand(0).unwrap().left().unwrap();
    let z3op = state.operand_to_bv(op);
    let source_size = z3op.get_size();
    let dest_size = get_dest_type(inst).into_int_type().get_bit_width();
    let dest = get_dest_name(inst);
    let z3dest = BV::new_const(state.ctx, dest, dest_size);
    state.assert(&z3dest._eq(&z3op.sign_ext(dest_size - source_size)));
    state.add_bv_var(inst, z3dest);
}

fn symex_trunc(state: &mut State, inst: InstructionValue) {
    debug!("Symexing trunc {}", &get_value_name(inst));
    assert_eq!(inst.get_num_operands(), 1);
    let op = inst.get_operand(0).unwrap().left().unwrap();
    let z3op = state.operand_to_bv(op);
    let dest_size = get_dest_type(inst).into_int_type().get_bit_width();
    let dest = get_dest_name(inst);
    let z3dest = BV::new_const(state.ctx, dest, dest_size);
    state.assert(&z3dest._eq(&z3op.extract(dest_size-1, 0)));
    state.add_bv_var(inst, z3dest);
}

fn symex_bitcast(state: &mut State, inst: InstructionValue) {
    debug!("Symexing bitcast {}", &get_value_name(inst));
    assert_eq!(inst.get_num_operands(), 1);
    let op = inst.get_operand(0).unwrap().left().unwrap();
    let z3op = state.operand_to_bv(op);
    let dest = get_dest_name(inst);
    let z3dest = BV::new_const(state.ctx, dest, z3op.get_size());
    state.assert(&z3dest._eq(&z3op));  // from Z3's perspective the bitcast is simply a no-op; the bit patterns are equal
    state.add_bv_var(inst, z3dest);
}

fn symex_load(state: &mut State, inst: InstructionValue) {
    debug!("Symexing load {}", &get_value_name(inst));
    assert_eq!(inst.get_num_operands(), 1);
    let addr = inst.get_operand(0).unwrap().left().unwrap();
    let z3addr = state.operand_to_bv(addr);
    let dest_size = get_dest_type(inst).into_int_type().get_bit_width();
    let dest = get_dest_name(inst);
    let z3dest = BV::new_const(state.ctx, dest, dest_size);
    state.assert(&z3dest._eq(&state.read(&z3addr, dest_size)));
    state.add_bv_var(inst, z3dest);
}

fn symex_store(state: &mut State, inst: InstructionValue) {
    debug!("Symexing store {}", &get_value_name(inst));
    assert_eq!(inst.get_num_operands(), 2);
    let val = inst.get_operand(0).unwrap().left().unwrap();
    let z3val = state.operand_to_bv(val);
    let addr = inst.get_operand(1).unwrap().left().unwrap();
    let z3addr = state.operand_to_bv(addr);
    state.write(&z3addr, z3val);
}

fn symex_gep(state: &mut State, inst: InstructionValue) {
    debug!("Symexing gep {}", &get_value_name(inst));
    assert_eq!(inst.get_num_operands(), 2);
    let base = inst.get_operand(0).unwrap().left().unwrap();
    let z3base = state.operand_to_bv(base);
    let index = inst.get_operand(1).unwrap().left().unwrap();
    let z3index = state.operand_to_bv(index);
    let dest = get_dest_name(inst);
    let z3dest = BV::new_const(state.ctx, dest, 64);  // dest is a pointer, so by our convention is 64 bits
    let dest_type = get_dest_type(inst);
    let type_pointed_to = dest_type.into_pointer_type().get_element_type();
    let size_pointed_to = match type_pointed_to {
        AnyTypeEnum::IntType(t) => t.get_bit_width(),
        AnyTypeEnum::PointerType(_) => 64,
        _ => unimplemented!("GEP with element type {:?}", type_pointed_to),
    };
    let total_offset = z3index.bvmul(&BV::from_u64(state.ctx, size_pointed_to.into(), z3index.get_size()));
    state.assert(&z3dest._eq(&z3base.bvadd(&total_offset)));
    state.add_bv_var(inst, z3dest);
}

fn symex_alloca(state: &mut State, inst: InstructionValue) {
    debug!("Symexing alloca {}", &get_value_name(inst));
    assert_eq!(inst.get_num_operands(), 1);
    let dest_type = get_dest_type(inst);
    let type_pointed_to = dest_type.into_pointer_type().get_element_type();
    let size_pointed_to = match type_pointed_to {
        AnyTypeEnum::IntType(t) => t.get_bit_width(),
        AnyTypeEnum::PointerType(_) => 64,
        _ => unimplemented!("Alloca for type {:?}", type_pointed_to),
    };
    let allocated = state.allocate(size_pointed_to.into());
    state.add_bv_var(inst, allocated);
}

fn symex_call(state: &mut State, inst: InstructionValue) {
    let inst: CallSiteValue = unsafe { std::mem::transmute(inst) };  // This InstructionValue is actually a CallSiteValue, but the current inkwell type system doesn't express this (?) so this seems to be the way to do it (?)
    debug!("Symexing call {}", &get_value_name(inst));
    let func: FunctionValue = inst.get_called_fn_value();
    let funcname = func.get_name().to_str().expect("Failed to convert CStr");
    if funcname.starts_with("llvm.") {
        return  // We ignore these llvm-internal functions
    }
    unimplemented!("Call of a function named {}", funcname);
}

// Returns the BV representing the return value
fn symex_return<'ctx>(state: &State<'ctx>, inst: InstructionValue) -> BV<'ctx> {
    debug!("Symexing return {}", &get_value_name(inst));
    assert_eq!(inst.get_num_operands(), 1);
    let rval = inst.get_operand(0).unwrap().left().unwrap();
    state.operand_to_bv(rval)
}

// Continues to the target of the Br (saving a backtracking point if necessary)
// and eventually returns the new BV representing the return value of the function
// (when it reaches the end of the function)
fn symex_br<'ctx>(state: &mut State<'ctx>, inst: InstructionValue, cur_bb: BasicBlock) -> BV<'ctx> {
    debug!("Symexing branch {}", &get_value_name(inst));
    match inst.get_num_operands() {
        1 => {
            // unconditional branch
            let bb = inst.get_operand(0).unwrap().right().expect("Single-operand Br but operand is not a BasicBlock");
            symex_from_bb(state, bb, Some(cur_bb))
        },
        3 => {
            // conditional branch
            let cond = inst.get_operand(0).unwrap().left().unwrap();
            let z3cond = state.operand_to_bool(cond.into_int_value());
            let true_feasible = state.check_with_extra_constraints(&[&z3cond]);
            let false_feasible = state.check_with_extra_constraints(&[&z3cond.not()]);
            // From empirical evidence, I guess get_operand(1) is the false branch and get_operand(2) is the true branch?
            let false_branch = 1;
            let true_branch = 2;
            if true_feasible && false_feasible {
                // for now we choose to explore true first, and backtrack to false if necessary
                state.save_backtracking_point(inst.get_operand(false_branch).unwrap().right().unwrap(), cur_bb, z3cond.not());
                state.assert(&z3cond);
                symex_from_bb(state, inst.get_operand(true_branch).unwrap().right().unwrap(), Some(cur_bb))
            } else if true_feasible {
                state.assert(&z3cond);  // unnecessary, but may help Z3 more than it hurts?
                symex_from_bb(state, inst.get_operand(true_branch).unwrap().right().unwrap(), Some(cur_bb))
            } else if false_feasible {
                state.assert(&z3cond.not());  // unnecessary, but may help Z3 more than it hurts?
                symex_from_bb(state, inst.get_operand(false_branch).unwrap().right().unwrap(), Some(cur_bb))
            } else if let Some((bb, prev_bb)) = state.revert_to_backtracking_point() {
                symex_from_bb(state, bb, Some(prev_bb))
            } else {
                panic!("All possible paths seem to be unsat");
            }
        },
        n => { unimplemented!("Br with {} operands", n); },
    }
}

fn symex_phi(state: &mut State, inst: InstructionValue, prev_bb: Option<BasicBlock>) {
    let inst: PhiValue = unsafe { std::mem::transmute(inst) };  // This InstructionValue is actually a PhiValue, but the current inkwell type system doesn't express this (?) so this seems to be the way to do it (?)
    debug!("Symexing phi {}", &get_value_name(inst));
    let prev_bb = prev_bb.expect("not yet implemented: starting in a block with Phi instructions");
    let pairs = PhiIterator::new(inst);
    let mut chosen_value = None;
    for (bve, bb) in pairs {
        if bb == prev_bb {
            chosen_value = Some(bve);
        }
    }
    let chosen_value = chosen_value.expect("Failed to find a Phi member matching previous BasicBlock");
    let z3value = state.operand_to_bv(chosen_value);
    state.add_bv_var(inst, z3value);
}

fn symex_select(state: &mut State, inst: InstructionValue) {
    debug!("Symexing select {}", &get_value_name(inst));
    assert_eq!(inst.get_num_operands(), 3);
    let cond = inst.get_operand(0).unwrap().left().unwrap();
    let firstop = inst.get_operand(1).unwrap().left().unwrap();
    let secondop = inst.get_operand(2).unwrap().left().unwrap();
    let z3cond = state.operand_to_bool(cond.into_int_value());
    let z3firstop = state.operand_to_bv(firstop);
    let z3secondop = state.operand_to_bv(secondop);
    let width = firstop.get_type().as_int_type().get_bit_width();
    assert_eq!(width, secondop.get_type().as_int_type().get_bit_width());
    let dest = get_dest_name(inst);
    let z3dest = BV::new_const(state.ctx, dest, width);
    let true_feasible = state.check_with_extra_constraints(&[&z3cond]);
    let false_feasible = state.check_with_extra_constraints(&[&z3cond.not()]);
    if true_feasible && false_feasible {
        state.assert(&z3dest._eq(&Bool::ite(&z3cond, &z3firstop, &z3secondop)));
    } else if true_feasible {
        state.assert(&z3cond);  // unnecessary, but may help Z3 more than it hurts?
        state.assert(&z3dest._eq(&z3firstop));
    } else if false_feasible {
        state.assert(&z3cond.not());  // unnecessary, but may help Z3 more than it hurts?
        state.assert(&z3dest._eq(&z3secondop));
    } else {
        unimplemented!("discovered we're unsat while checking a switch condition");
    }
    state.add_bv_var(inst, z3dest);
}