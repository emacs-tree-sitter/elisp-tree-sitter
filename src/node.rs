use emacs::{defun, Value, Result};

use tree_sitter::InputEdit;

use crate::types::*;

/// Exposes methods that return a node's property.
macro_rules! defun_node_props {
    ($($(#[$meta:meta])* $($lisp_name:literal)? fn $name:ident -> $type:ty $(; $into:ident)? )*) => {
        $(
            #[defun$((name = $lisp_name))?]
            $(#[$meta])*
            fn $name(node: &RNode) -> Result<$type> {
                Ok(node.borrow().$name()$(.$into())?)
            }
        )*
    };
}

/// Exposes methods that return another node.
macro_rules! defun_node_navs {
    ($($(#[$meta:meta])* $($lisp_name:literal)? fn $name:ident $( ( $( $param:ident $($into:ident)? : $type:ty ),* ) )?)*) => {
        $(
            #[defun$((name = $lisp_name))?]
            $(#[$meta])*
            fn $name(node: &RNode, $( $( $param : $type ),* )? ) -> Result<Option<RNode>> {
                Ok(node.borrow().$name( $( $( $param $(.$into())? ),* )? ).map(|other| {
                    node.map(|_| other)
                }))
            }
        )*
    };
}

defun_node_props! {
    /// Return NODE's type-id.
    "node-type-id" fn kind_id -> u16

    /// Return NODE's type.
    "node-type" fn kind -> &'static str

    // Predicates ----------------------------------------------------------------------------------

    /// Return t if NODE is 'named'.
    /// Named nodes correspond to named rules in the grammar, whereas anonymous nodes
    /// correspond to string literals in the grammar.
    "node-named-p" fn is_named -> bool

    /// Return t if NODE is 'extra'.
    /// Extra nodes represent things like comments, which are not required the grammar,
    /// but can appear anywhere.
    "node-extra-p" fn is_extra -> bool

    /// Return t if NODE represents a syntax error.
    /// Syntax errors represent parts of the code that could not be incorporated into a
    /// valid syntax tree.
    "node-error-p" fn is_error -> bool

    /// Return t if NODE is 'missing'.
    /// Missing nodes are inserted by the parser in order to recover from certain kinds
    /// of syntax errors.
    "node-missing-p" fn is_missing -> bool

    /// Return t if NODE has been edited.
    "node-has-changes-p" fn has_changes -> bool

    /// Return t if NODE represents a syntax error or contains any syntax errors.
    "node-has-error-p" fn has_error -> bool

    // Position ------------------------------------------------------------------------------------

    /// Return NODE's start byte position.
    "node-start-byte" fn start_byte -> BytePos; into

    /// Return NODE's start point, as a [LINE-NUMBER BYTE-COLUMN] vector.
    "node-start-point" fn start_position -> Point; into

    /// Return NODE's end byte position.
    "node-end-byte" fn end_byte -> BytePos; into

    /// Return NODE's end point, as a [LINE-NUMBER BYTE-COLUMN] vector.
    "node-end-point" fn end_position -> Point; into

    /// Return a vector of NODE's [START-BYTEPOS END-BYTEPOS START-POINT END-POINT].
    "node-range" fn range -> Range; into

    // Counting child nodes ------------------------------------------------------------------------

    /// Return NODE's number of children.
    "count-children" fn child_count -> usize

    /// Return NODE's number of named children.
    "count-named-children" fn named_child_count -> usize
}

/// Apply FUNCTION to each of NODE's children, for side effects only.
#[defun]
fn mapc_children(function: Value, node: &RNode) -> Result<()> {
    let inner = node.borrow();
    // TODO: Reuse cursor.
    let cursor = &mut inner.walk();
    for child in inner.children(cursor) {
        let child = node.map(|_| child);
        function.call((child,))?;
    }
    Ok(())
}

// TODO: named_children.
// TODO: children_by_field_name.
// TODO: children_by_field_id.

defun_node_navs! {
    /// Return NODE's parent node.
    "get-parent" fn parent

    // Child ---------------------------------------------------------------------------------------

    /// Return NODE's child at the given 0-based index.
    "get-nth-child" fn child(i: usize)

    /// Return NODE's named child at the given 0-based index.
    "get-nth-named-child" fn named_child(i: usize)

    /// Return NODE's child with the given FIELD-NAME.
    "get-child-by-field-name" fn child_by_field_name(field_name: String)

    /// Return NODE's child with the given numerical FIELD-ID.
    "get-child-by-field-id" fn child_by_field_id(field_id: u16)

    // Sibling -------------------------------------------------------------------------------------

    /// Return NODE's next sibling.
    "get-next-sibling" fn next_sibling

    /// Return NODE's previous sibling.
    "get-prev-sibling" fn prev_sibling

    /// Return NODE's next named sibling.
    "get-next-named-sibling" fn next_named_sibling

    /// Return NODE's previous named sibling.
    "get-prev-named-sibling" fn prev_named_sibling

    // Descendant ----------------------------------------------------------------------------------

    /// Return the smallest node within NODE that spans the given range of byte
    /// positions.
    "get-descendant-for-byte-range" fn descendant_for_byte_range(start into: BytePos, end into: BytePos)

    /// Return the smallest node within NODE that spans the given point range.
    "get-descendant-for-point-range" fn descendant_for_point_range(start into: Point, end into: Point)

    /// Return the smallest named node within NODE that spans the given range of byte
    /// positions.
    "get-named-descendant-for-byte-range" fn named_descendant_for_byte_range(start into: BytePos, end into: BytePos)

    /// Return the smallest named node within NODE that spans the given point range.
    "get-named-descendant-for-point-range" fn named_descendant_for_point_range(start into: Point, end into: Point)
}

defun_node_props! {
    /// Return the sexp representation of NODE, in a string.
    "node-to-sexp" fn to_sexp -> String
}

/// Edit NODE to keep it in sync with source code that has been edited.
///
/// You must describe the edit both in terms of byte positions and in terms of
/// [LINE-NUMBER BYTE-COLUMN] coordinates.
///
/// LINE-NUMBER should be the number returned by `line-number-at-pos', which counts
/// from 1.
///
/// BYTE-COLUMN should count from 0, like Emacs's `current-column'. However, unlike
/// that function, it should count bytes, instead of displayed glyphs.
///
/// This function is only rarely needed. When you edit a syntax tree, all of the
/// nodes that you retrieve from the tree afterward will already reflect the edit.
/// You only need to use this function when you have a node that you want to keep
/// and continue to use after an edit.
#[defun]
fn edit_node(
    node: &mut RNode,
    start_bytepos: BytePos,
    old_end_bytepos: BytePos,
    new_end_bytepos: BytePos,
    start_point: Point,
    old_end_point: Point,
    new_end_point: Point,
) -> Result<()> {
    let edit = InputEdit {
        start_byte: start_bytepos.into(),
        old_end_byte: old_end_bytepos.into(),
        new_end_byte: new_end_bytepos.into(),
        start_position: start_point.into(),
        old_end_position: old_end_point.into(),
        new_end_position: new_end_point.into(),
    };
    node.borrow_mut().edit(&edit);
    Ok(())
}
