use std::{
    cell::{Ref, RefCell, RefMut},
    mem,
    ops::{Deref, DerefMut},
};

use emacs::{defun, Env, IntoLisp, Result, Value, GlobalRef};
use tree_sitter::{InputEdit, Node, Tree};

use crate::{
    types::{self, BytePos, Point, Shared, Range},
    lang::Language,
};

// -------------------------------------------------------------------------------------------------

/// Wrapper around `tree_sitter::Node` that can have 'static lifetime, by keeping a ref-counted
/// reference to the underlying tree.
#[derive(Clone)]
pub struct RNode {
    tree: Shared<Tree>,
    inner: Node<'static>,
}

impl_pred!(node_p, &RefCell<RNode>);

pub struct RNodeBorrow<'e> {
    #[allow(unused)]
    reft: Ref<'e, Tree>,
    node: &'e Node<'e>,
}

impl<'e> Deref for RNodeBorrow<'e> {
    type Target = Node<'e>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.node
    }
}

pub struct RNodeBorrowMut<'e> {
    #[allow(unused)]
    reft: RefMut<'e, Tree>,
    node: Node<'e>,
}

impl<'e> Deref for RNodeBorrowMut<'e> {
    type Target = Node<'e>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

impl<'e> DerefMut for RNodeBorrowMut<'e> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.node
    }
}

impl PartialEq for RNode {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl IntoLisp<'_> for RNode {
    fn into_lisp(self, env: &Env) -> Result<Value> {
        RefCell::new(self).into_lisp(env)
    }
}

impl RNode {
    pub fn new<'e, F: FnOnce(&'e Tree) -> Node<'e>>(tree: Shared<Tree>, f: F) -> Self {
        let rtree = unsafe { types::erase_lifetime(&*tree.borrow()) };
        let inner = unsafe { mem::transmute(f(rtree)) };
        Self { tree, inner }
    }

    pub fn clone_tree(&self) -> Shared<Tree> {
        self.tree.clone()
    }

    pub fn map<'e, F: FnOnce(&Node<'e>) -> Node<'e>>(&self, f: F) -> Self {
        Self::new(self.clone_tree(), |_| f(&self.inner))
    }

    #[inline]
    pub fn borrow(&self) -> RNodeBorrow {
        let reft = self.tree.borrow();
        let node = &self.inner;
        RNodeBorrow { reft, node }
    }

    #[inline]
    pub fn borrow_mut(&mut self) -> RNodeBorrowMut {
        let reft = self.tree.borrow_mut();
        let node = self.inner;
        RNodeBorrowMut { reft, node }
    }
}

// -------------------------------------------------------------------------------------------------

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

emacs::use_symbols!(ERROR);

/// Return NODE's type, as a symbol (named node), or a string (anonymous node).
///
/// If NODE is a named node, its type is a symbol. For example: 'identifier, 'block.
/// If NODE is an anonymous node, its type is a string. For example: "if", "else".
#[defun]
fn node_type(node: &RNode) -> Result<&'static GlobalRef> {
    Ok(node.borrow().lisp_type())
}

pub(crate) trait LispUtils {
    fn lisp_type(&self) -> &'static GlobalRef;
    fn lisp_byte_range<'e>(&self, env: &'e Env) -> Result<Value<'e>>;
    fn lisp_start_byte(&self) -> BytePos;
    fn lisp_end_byte(&self) -> BytePos;
    fn lisp_start_point(&self) -> Point;
    fn lisp_end_point(&self) -> Point;
    fn lisp_range(&self) -> Range;
}

impl<'n> LispUtils for Node<'n> {
    #[inline]
    fn lisp_type(&self) -> &'static GlobalRef {
        let language: Language = self.language().into();
        if self.is_error() {
            ERROR
        } else {
            &language.info().node_type(self.kind_id()).expect("Failed to get node type from id")
        }
    }

    #[inline]
    fn lisp_byte_range<'e>(&self, env: &'e Env) -> Result<Value<'e>> {
        let beg: BytePos = self.start_byte().into();
        let end: BytePos = self.end_byte().into();
        env.cons(beg, end)
    }

    #[inline]
    fn lisp_start_byte(&self) -> BytePos {
        self.start_byte().into()
    }

    #[inline]
    fn lisp_end_byte(&self) -> BytePos {
        self.end_byte().into()
    }

    #[inline]
    fn lisp_start_point(&self) -> Point {
        self.start_position().into()
    }

    #[inline]
    fn lisp_end_point(&self) -> Point {
        self.end_position().into()
    }

    #[inline]
    fn lisp_range(&self) -> Range {
        self.range().into()
    }}

defun_node_props! {
    /// Return NODE's numeric type-id.
    "node-type-id" fn kind_id -> u16

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

    /// Return NODE's start point, in the form of (LINE-NUMBER . BYTE-COLUMN).
    "node-start-point" fn start_position -> Point; into

    /// Return NODE's end byte position.
    "node-end-byte" fn end_byte -> BytePos; into

    /// Return NODE's end point, in the form of (LINE-NUMBER . BYTE-COLUMN).
    "node-end-point" fn end_position -> Point; into

    /// Return a vector of NODE's [START-BYTEPOS END-BYTEPOS START-POINT END-POINT].
    "node-range" fn range -> Range; into

    // Counting child nodes ------------------------------------------------------------------------

    /// Return NODE's number of children.
    "count-children" fn child_count -> usize

    /// Return NODE's number of named children.
    "count-named-children" fn named_child_count -> usize
}

/// Return NODE's (START-BYTEPOS . END-BYTEPOS).
#[defun]
fn node_byte_range<'e>(env: &'e Env, node: &RNode) -> Result<Value<'e>> {
    node.borrow().lisp_byte_range(env)
}

/// Return t if two nodes are identical.
#[defun]
fn node_eq(node1: &RNode, node2: &RNode) -> Result<bool> {
    Ok(node1 == node2)
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

    /// Return NODE's child with the given FIELD-NAME string.
    "-get-child-by-field-name" fn child_by_field_name(field_name: String)

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
/// (LINE-NUMBER . BYTE-COLUMN) coordinates.
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
