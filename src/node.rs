use std::{mem, cell::RefCell};

use emacs::{defun, Value, Result, IntoLisp};

use tree_sitter::{Node, InputEdit};

use crate::types::{WrappedNode, Range, Point};

macro_rules! prop {
    ($($lisp_name:literal)? fn $name:ident -> $type:ty) => {
        #[defun$((name = $lisp_name))?]
        fn $name(node: &WrappedNode) -> Result<$type> {
            Ok(node.inner().$name())
        }
    };
    ($($lisp_name:literal)? fn $name:ident -> into $type:ty) => {
        #[defun$((name = $lisp_name))?]
        fn $name(node: &WrappedNode) -> Result<$type> {
            Ok(node.inner().$name().into())
        }
    };
}

/// Exposes a method that returns a(nother) node.
macro_rules! node {
    ($($lisp_name:literal)? fn $name:ident $( ( $( $param:ident $($into:ident)? : $type:ty ),* ) )? ) => {
        #[defun$((name = $lisp_name))?]
        fn $name(node: &WrappedNode, $( $( $param : $type ),* )? ) -> Result<Option<RefCell<WrappedNode>>> {
            Ok(node.inner().$name( $( $( $param $(.$into())? ),* )? ).map(|other| {
                RefCell::new(unsafe { node.wrap(other) })
            }))
        }
    };
}

prop!("node-kind-id" fn kind_id -> u16);
prop!("node-kind" fn kind -> &'static str);

prop!("node-named-p" fn is_named -> bool);
prop!("node-extra-p" fn is_extra -> bool);
prop!("node-error-p" fn is_error -> bool);
prop!("node-missing-p" fn is_missing -> bool);
prop!("node-has-changes-p" fn has_changes -> bool);
prop!("node-has-error-p" fn has_error -> bool);

prop!("node-start-byte" fn start_byte -> usize);
prop!("node-start-point" fn start_position -> into Point);
prop!("node-end-byte" fn end_byte -> usize);
prop!("node-end-point" fn end_position -> into Point);
prop!("node-range" fn range -> into Range);

prop!("count-children" fn child_count -> usize);
prop!("count-named-children" fn named_child_count -> usize);

#[defun]
fn mapc_children(node: &WrappedNode, f: Value) -> Result<()> {
    let tree = &node.tree;
    let env = f.env;
    for child in node.inner().children() {
        let child = RefCell::new(unsafe { node.wrap(child) });
        env.call("funcall", &[f, child.into_lisp(env)?])?;
    }
    Ok(())
}

node!("get-nth-child" fn child(i: usize));
node!("get-nth-named-child" fn named_child(i: usize));
node!("get-child-by-field-name" fn child_by_field_name(field_name: String));

node!("get-parent" fn parent);

node!("get-next-sibling" fn next_sibling);
node!("get-prev-sibling" fn prev_sibling);
node!("get-next-named-sibling" fn next_named_sibling);
node!("get-prev-named-sibling" fn prev_named_sibling);

node!("get-descendant-for-byte-range" fn descendant_for_byte_range(start: usize, end: usize));
node!("get-descendant-for-point-range" fn descendant_for_point_range(start into: Point, end into: Point));
node!("get-named-descendant-for-byte-range" fn named_descendant_for_byte_range(start: usize, end: usize));
node!("get-named-descendant-for-point-range" fn named_descendant_for_point_range(start into: Point, end into: Point));

prop!("node-to-sexp" fn to_sexp -> String);

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
