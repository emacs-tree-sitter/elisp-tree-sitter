use std::{
    mem,
    cell::RefCell,
};

use emacs::{defun, Value, Result, IntoLisp};

use tree_sitter::{Node, InputEdit};

use crate::types::{WrappedNode, Range, Point};

macro_rules! getter {
    ($name:ident -> $type:ty) => {
        #[defun]
        fn $name(node: &WrappedNode) -> Result<$type> {
            Ok(node.inner().$name())
        }
    };
    ($name:ident -> into $type:ty) => {
        #[defun]
        fn $name(node: &WrappedNode) -> Result<$type> {
            Ok(node.inner().$name().into())
        }
    };
    ($name:ident as $lisp_name:ident -> $type:ty) => {
        #[defun]
        fn $lisp_name(node: &WrappedNode) -> Result<$type> {
            Ok(node.inner().$name())
        }
    };
    ($name:ident as $lisp_name:ident -> into $type:ty) => {
        #[defun]
        fn $lisp_name(node: &WrappedNode) -> Result<$type> {
            Ok(node.inner().$name().into())
        }
    };
}

/// Exposes a method that returns a(nother) node.
macro_rules! walker {
    ($name:ident) => {
        #[defun]
        fn $name(node: &WrappedNode) -> Result<Option<RefCell<WrappedNode>>> {
            Ok(node.inner().$name().map(|other| {
                RefCell::new(unsafe { node.wrap(other) })
            }))
        }
    };
    ($name:ident, $($param:ident $($into:ident)? : $type:ty),*) => {
        #[defun]
        fn $name(node: &WrappedNode, $($param: $type),*) -> Result<Option<RefCell<WrappedNode>>> {
            Ok(node.inner().$name($($param $(.$into())? ),*).map(|other| {
                RefCell::new(unsafe { node.wrap(other) })
            }))
        }
    };
    ($name:ident ( $($param:ident $($into:ident)? : $type:ty),* )) => {
        #[defun]
        fn $name(node: &WrappedNode, $($param: $type),*) -> Result<Option<RefCell<WrappedNode>>> {
            Ok(node.inner().$name($($param $(.$into())? ),*).map(|other| {
                RefCell::new(unsafe { node.wrap(other) })
            }))
        }
    };
}

getter!(kind_id as node_symbol -> u16);
getter!(kind as node_type -> &'static str);

getter!(is_named -> bool);
getter!(is_extra -> bool);
getter!(has_changes -> bool);
getter!(has_error -> bool);
getter!(is_error -> bool);
getter!(is_missing -> bool);

getter!(start_byte -> usize);
getter!(end_byte -> usize);
getter!(range -> into Range);
getter!(start_position as start_point -> into Point);
getter!(end_position as end_point -> into Point);

walker!(child(i: usize));
walker!(child_by_field_name(field_name: String));
getter!(child_count -> usize);

#[defun]
fn for_each_child_node(node: &WrappedNode, f: Value) -> Result<()> {
    let tree = &node.tree;
    let env = f.env;
    for child in node.inner().children() {
        let child = RefCell::new(unsafe { node.wrap(child) });
        env.call("funcall", &[f, child.into_lisp(env)?])?;
    }
    Ok(())
}

walker!(named_child, i: usize);
getter!(named_child_count -> usize);

walker!(parent);

walker!(next_sibling);
walker!(prev_sibling);
walker!(next_named_sibling);
walker!(prev_named_sibling);

walker!(descendant_for_byte_range(start: usize, end: usize));
walker!(named_descendant_for_byte_range(start: usize, end: usize));
walker!(descendant_for_point_range(start into: Point, end into: Point));
walker!(named_descendant_for_point_range(start into: Point, end into: Point));

getter!(to_sexp as node_to_sexp -> String);

#[allow(clippy::too_many_arguments)]
#[defun]
fn edit_node(
    node: &mut WrappedNode,
    start_byte: usize,
    old_end_byte: usize,
    new_end_byte: usize,
    start_row: usize,
    start_column: usize,
    old_end_row: usize,
    old_end_column: usize,
    new_end_row: usize,
    new_end_column: usize,
) -> Result<()> {
    let edit = InputEdit {
        start_byte,
        old_end_byte,
        new_end_byte,
        start_position: tree_sitter::Point { row: start_row, column: start_column },
        old_end_position: tree_sitter::Point { row: old_end_row, column: old_end_column },
        new_end_position: tree_sitter::Point { row: new_end_row, column: new_end_column },
    };
    node.inner_mut().edit(&edit);
    Ok(())
}
