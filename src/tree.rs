use emacs::{defun, Value, Result};

use std::{
    cell::RefCell,
    rc::Rc,
    mem,
};

use tree_sitter::{InputEdit, Point};

use super::types::{SharedTree, WrappedNode};

#[defun]
fn tree_to_sexp(tree: Value) -> Result<String> {
    let tree: &SharedTree = tree.into_rust()?;
    Ok(tree.try_borrow()?.root_node().to_sexp())
}

#[defun(user_ptr)]
fn root_node(tree: Value) -> Result<WrappedNode> {
    let tree: &SharedTree = tree.into_rust()?;
    Ok(unsafe { WrappedNode::new(tree, tree.try_borrow()?.root_node()) })
}

#[defun]
fn edit(
    tree: Value,
    start_byte: i64,
    old_end_byte: i64,
    new_end_byte: i64,
    start_row: i64,
    start_column: i64,
    old_end_row: i64,
    old_end_column: i64,
    new_end_row: i64,
    new_end_column: i64,
) -> Result<()> {
    let tree: &SharedTree = tree.into_rust()?;
    let edit = InputEdit {
        start_byte: start_byte as usize,
        old_end_byte: old_end_byte as usize,
        new_end_byte: new_end_byte as usize,
        start_position: Point { row: start_row as usize, column: start_column as usize },
        old_end_position: Point { row: old_end_row as usize, column: old_end_column as usize },
        new_end_position: Point { row: new_end_row as usize, column: new_end_column as usize },
    };
    tree.try_borrow_mut()?.edit(&edit);
    Ok(())
}
