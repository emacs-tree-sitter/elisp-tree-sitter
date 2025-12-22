use std::{
    cell::{Ref, RefCell},
    mem,
    ops::{Deref, DerefMut},
};

use emacs::{defun, Result, Value, Env, GlobalRef, Vector, IntoLisp, FromLisp};
use tree_sitter::{Tree, TreeCursor, Node};

use crate::{
    types::{self, Shared, BytePos},
    node::{RNode, LispUtils},
    lang::Language,
};

emacs::use_symbols! {
    wrong_type_argument
    tree_or_node_p

    _type        => ":type"
    _named_p     => ":named-p"
    _extra_p     => ":extra-p"
    _error_p     => ":error-p"
    _missing_p   => ":missing-p"
    _has_error_p => ":has-error-p"
    _start_byte  => ":start-byte"
    _start_point => ":start-point"
    _end_byte    => ":end-byte"
    _end_point   => ":end-point"
    _range       => ":range"
    _byte_range  => ":byte-range"

    _field       => ":field"
    _depth       => ":depth"
}

// -------------------------------------------------------------------------------------------------

/// Wrapper around `tree_sitter::TreeCursor` that can have 'static lifetime, by keeping a
/// ref-counted reference to the underlying tree.
#[derive(Clone)]
pub struct RCursor {
    tree: Shared<Tree>,
    inner: TreeCursor<'static>,
}

impl_pred!(cursor_p, &RefCell<RCursor>);

pub struct RCursorBorrow<'e> {
    #[allow(unused)]
    reft: Ref<'e, Tree>,
    cursor: &'e TreeCursor<'e>,
}

impl<'e> Deref for RCursorBorrow<'e> {
    type Target = TreeCursor<'e>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.cursor
    }
}

pub struct RCursorBorrowMut<'e> {
    #[allow(unused)]
    reft: Ref<'e, Tree>,
    cursor: &'e mut TreeCursor<'e>,
}

impl<'e> Deref for RCursorBorrowMut<'e> {
    type Target = TreeCursor<'e>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.cursor
    }
}

impl<'e> DerefMut for RCursorBorrowMut<'e> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.cursor
    }
}

impl RCursor {
    pub fn new<'e, F: FnOnce(&'e Tree) -> TreeCursor<'e>>(tree: Shared<Tree>, f: F) -> Self {
        let rtree = unsafe { types::erase_lifetime(&*tree.borrow()) };
        let inner = unsafe { mem::transmute(f(rtree)) };
        Self { tree, inner }
    }

    pub fn clone_tree(&self) -> Shared<Tree> {
        self.tree.clone()
    }

    #[inline]
    pub fn borrow(&self) -> RCursorBorrow {
        let reft = self.tree.borrow();
        let cursor = &self.inner;
        RCursorBorrow { reft, cursor }
    }

    #[inline]
    pub fn borrow_mut(&mut self) -> RCursorBorrowMut {
        let reft: Ref<Tree> = self.tree.borrow();
        // XXX: Explain the safety here.
        let cursor: &mut _ = unsafe { mem::transmute(&mut self.inner) };
        RCursorBorrowMut { reft, cursor }
    }
}

pub enum TreeOrNode<'e> {
    Tree(&'e Shared<Tree>),
    Node(&'e RefCell<RNode>),
}

impl<'e> FromLisp<'e> for TreeOrNode<'e> {
    fn from_lisp(value: Value<'e>) -> Result<Self> {
        if let Ok(value) = value.into_rust() {
            return Ok(Self::Tree(value));
        }
        if let Ok(value) = value.into_rust() {
            return Ok(Self::Node(value));
        }
        value.env.signal(wrong_type_argument, (tree_or_node_p, value))
    }
}

impl<'e> TreeOrNode<'e> {
    fn walk(&self) -> RCursor {
        match *self {
            Self::Tree(tree) => RCursor::new(tree.clone(), |tree| tree.walk()),
            Self::Node(node) => {
                let node = node.borrow();
                RCursor::new(node.clone_tree(), |_| node.borrow().walk())
            }
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Create a new cursor starting from the given TREE-OR-NODE.
///
/// A cursor allows you to walk a syntax tree more efficiently than is possible
/// using `tsc-get-' functions. It is a mutable object that is always on a certain
/// syntax node, and can be moved imperatively to different nodes.
///
/// If a tree is given, the returned cursor starts on its root node.
#[defun(user_ptr)]
fn make_cursor(tree_or_node: TreeOrNode) -> Result<RCursor> {
    Ok(tree_or_node.walk())
}

/// Return the field id of CURSOR's current node.
/// Return nil if the current node doesn't have a field.
#[defun]
fn current_field_id(cursor: &RCursor) -> Result<Option<u16>> {
    Ok(cursor.borrow().field_id().map(|id| id.get()))
}

/// Return the field associated with CURSOR's current node, as a keyword.
/// Return nil if the current node is not associated with a field.
#[defun]
fn current_field(cursor: &RCursor) -> Result<Option<&'static GlobalRef>> {
    let cursor = cursor.borrow();
    let language: Language = cursor.reft.language().clone().into();
    Ok(cursor.field_id().and_then(|id| language.info().field_name(id.into())))
}

macro_rules! defun_cursor_walks {
    ($($(#[$meta:meta])* $($lisp_name:literal)? fn $name:ident $( ( $( $param:ident $($into:ident)? : $itype:ty ),* ) )? -> $type:ty)*) => {
        $(
            $(#[$meta])*
            #[defun$((name = $lisp_name))?]
            fn $name(cursor: &mut RCursor, $( $( $param: $itype ),* )? ) -> Result<$type> {
                Ok(cursor.borrow_mut().$name( $( $( $param $(.$into())? ),* )? ))
            }
        )*
    };
}

defun_cursor_walks! {
    /// Move CURSOR to the first child of its current node.
    /// Return t if CURSOR successfully moved, nil if there were no children.
    fn goto_first_child -> bool

    /// Move CURSOR to the parent node of its current node.
    /// Return t if CURSOR successfully moved, nil if it was already on the root node.
    fn goto_parent -> bool

    /// Move CURSOR to the next sibling of its current node.
    /// Return t if CURSOR successfully moved, nil if there was no next sibling node.
    fn goto_next_sibling -> bool

    /// Move CURSOR to the first child that extends beyond the given BYTEPOS.
    /// Return the index of the child node if one was found, nil otherwise.
    "goto-first-child-for-byte" fn goto_first_child_for_byte(bytepos into: BytePos) -> Option<usize>
}

/// Re-initialize CURSOR to start at a different NODE.
#[defun]
fn reset_cursor(cursor: &mut RCursor, node: &RNode) -> Result<()> {
    Ok(cursor.borrow_mut().reset(*node.borrow()))
}

// -------------------------------------------------------------------------------------------------

enum TraversalState {
    Start,
    Down,
    Right,
    Done,
}

use TraversalState::*;

struct DepthFirstIterator {
    cursor: RCursor,
    state: TraversalState,
    depth: usize,
}

// TODO: Provide a function to move backward.
impl DepthFirstIterator {
    fn new(tree_or_node: TreeOrNode) -> Self {
        Self { cursor: tree_or_node.walk(), state: Start, depth: 0 }
    }

    #[inline]
    fn item(&self) -> Option<(RNode, usize)> {
        Some((RNode::new(self.cursor.clone_tree(), |_| self.cursor.borrow().node()), self.depth))
    }

    fn close(&mut self) {
        self.state = Done;
    }
}

impl Iterator for DepthFirstIterator {
    type Item = (RNode, usize);

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            Start => {
                self.state = Down;
                self.item()
            }
            Down => {
                if self.cursor.borrow_mut().goto_first_child() {
                    self.depth += 1;
                    self.item()
                } else {
                    self.state = Right;
                    self.next()
                }
            }
            Right => {
                if self.cursor.borrow_mut().goto_next_sibling() {
                    self.state = Down;
                    self.item()
                } else if self.cursor.borrow_mut().goto_parent() {
                    self.depth -= 1;
                    self.next()
                } else {
                    self.state = Done;
                    self.next()
                }
            }
            Done => None,
        }
    }
}

/// Create a new depth-first iterator from the given TREE-OR-NODE.
/// The traversal is pre-order.
#[defun(user_ptr)]
fn _iter(tree_or_node: TreeOrNode) -> Result<DepthFirstIterator> {
    Ok(DepthFirstIterator::new(tree_or_node))
}

/// Move ITERATOR to the next node.
/// Return t if ITERATOR successfully moved, nil if there was no next node, or if
/// ITERATOR was closed.
#[defun]
fn _iter_next(iterator: &mut DepthFirstIterator) -> Result<bool> {
    Ok(iterator.next().is_some())
}

/// Close ITERATOR.
#[defun]
fn _iter_close(iterator: &mut DepthFirstIterator) -> Result<()> {
    Ok(iterator.close())
}

#[derive(Clone, Copy)]
enum VectorOrKeyword<'e> {
    Vector(Vector<'e>),
    Keyword(Value<'e>),
}

impl<'e> FromLisp<'e> for VectorOrKeyword<'e> {
    fn from_lisp(value: Value<'e>) -> Result<Self> {
        if let Ok(value) = value.into_rust::<Vector>() {
            Ok(Self::Vector(value))
        } else {
            // TODO: Verify that it's a valid node property
            Ok(Self::Keyword(value))
        }
    }
}

/// Return the properties of ITERATOR's current node, or the node itself.
///
/// If PROPS is a vector of property names, return a vector containing the node's
/// corresponding properties. If OUTPUT is also non-nil, it must be a vector of the
/// same length, where the properties will be written into.
///
/// If PROPS is a single property name, return that property.
///
/// If PROPS is nil, return the node itself.
///
/// See `tsc-valid-node-props' for the list of available properties.
#[defun]
fn _iter_current_node<'e>(
    iterator: &mut DepthFirstIterator,
    props: Option<VectorOrKeyword<'e>>,
    output: Option<Vector<'e>>,
    env: &'e Env,
) -> Result<Value<'e>> {
    let cursor = &iterator.cursor;
    match props {
        Some(VectorOrKeyword::Keyword(prop)) if prop.eq(_depth.bind(env)) => {
            iterator.depth.into_lisp(env)
        }
        _ => {
            let result = _current_node(cursor, props, output, env)?;
            if let Some(VectorOrKeyword::Vector(props)) = props {
                if let Some(output) = output {
                    for (i, prop) in props.into_iter().enumerate() {
                        if prop.eq(_depth.bind(env)) {
                            output.set(i, iterator.depth)?;
                        }
                    }
                } else {
                    todo!()
                }
            }
            Ok(result)
        }
    }
}

/// Move ITERATOR to the next node, and retrieve its properties, or the node itself.
///
/// This a combination of `tsc--iter-next' and `tsc--iter-current-node'.
#[defun]
fn _iter_next_node<'e>(
    iterator: &mut DepthFirstIterator,
    props: Option<VectorOrKeyword<'e>>,
    output: Option<Vector<'e>>,
    env: &'e Env,
) -> Result<Option<Value<'e>>> {
    if iterator.next().is_some() {
        Ok(Some(_iter_current_node(iterator, props, output, env)?))
    } else {
        Ok(None)
    }
}

fn get<'e>(prop: Value<'e>, node: Node, cursor: &RCursor) -> Result<Value<'e>> {
    macro_rules! sugar {
        ($prop:ident, $env:ident) => {
            macro_rules! eq {
                ($name:ident) => {
                    $prop.eq($name.bind($env))
                };
            }
        };
    }
    let env = prop.env;
    sugar!(prop, env);
    if eq!(_type) {
        node.lisp_type().into_lisp(env)
    } else if eq!(_byte_range) {
        node.lisp_byte_range(env)
    } else if eq!(_start_byte) {
        node.lisp_start_byte().into_lisp(env)
    } else if eq!(_end_byte) {
        node.lisp_end_byte().into_lisp(env)
    } else if eq!(_field) {
        current_field(cursor)?.into_lisp(env)
    } else if eq!(_named_p) {
        node.is_named().into_lisp(env)
    } else if eq!(_extra_p) {
        node.is_extra().into_lisp(env)
    } else if eq!(_error_p) {
        node.is_error().into_lisp(env)
    } else if eq!(_missing_p) {
        node.is_missing().into_lisp(env)
    } else if eq!(_has_error_p) {
        node.has_error().into_lisp(env)
    } else if eq!(_start_point) {
        node.lisp_start_point().into_lisp(env)
    } else if eq!(_end_point) {
        node.lisp_end_point().into_lisp(env)
    } else if eq!(_range) {
        node.lisp_range().into_lisp(env)
    } else {
        // FIX: Signal an error instead.
        ().into_lisp(env)
    }
}

/// Return the properties of CURSOR's current node, or the node itself.
///
/// If PROPS is a vector of property names, return a vector containing the node's
/// corresponding properties. If OUTPUT is also non-nil, it must be a vector of the
/// same length, where the properties will be written into.
///
/// If PROPS is a single property name, return that property.
///
/// If PROPS is nil, return the node itself.
///
/// See `tsc-valid-node-props' for the list of available properties.
#[defun]
fn _current_node<'e>(
    cursor: &RCursor,
    props: Option<VectorOrKeyword<'e>>,
    output: Option<Vector<'e>>,
    env: &'e Env,
) -> Result<Value<'e>> {
    let node = cursor.borrow().node();
    match props {
        None => RNode::new(cursor.clone_tree(), |_| node).into_lisp(env),
        Some(VectorOrKeyword::Vector(props)) => {
            let result = match output {
                None => env.make_vector(props.len(), ())?,
                Some(output) => output,
            };
            for (i, prop) in props.into_iter().enumerate() {
                result.set(i, get(prop, node, cursor)?)?;
            }
            result.into_lisp(env)
        }
        Some(VectorOrKeyword::Keyword(prop)) => get(prop, node, cursor),
    }
}

/// Actual logic of `tsc-traverse-mapc'. The wrapper is needed because
/// `emacs-module-rs' doesn't currently support optional arguments.
#[defun]
fn _traverse_mapc(
    func: Value,
    tree_or_node: TreeOrNode,
    props: Option<VectorOrKeyword>,
) -> Result<()> {
    let mut iterator = DepthFirstIterator::new(tree_or_node);
    let env = func.env;
    let mut output = None;
    let mut depth_indexes = Vec::with_capacity(1);
    let mut depth = false;
    match props {
        Some(VectorOrKeyword::Vector(props)) => {
            output = Some(env.make_vector(props.len(), ())?);
            for (i, prop) in props.into_iter().enumerate() {
                if prop.eq(_depth.bind(env)) {
                    depth_indexes.push(i)
                }
            }
        }
        Some(VectorOrKeyword::Keyword(prop)) if prop.eq(_depth.bind(env)) => {
            depth = true;
        }
        _ => {}
    }
    // Can't use a for loop because we need to access the cursor to process each item.
    let mut item: Option<(RNode, usize)> = iterator.next();
    while item.is_some() {
        let result = if depth {
            iterator.depth.into_lisp(env)?
        } else {
            let result = _current_node(&iterator.cursor, props, output, env)?;
            if let Some(output) = output {
                for i in &depth_indexes {
                    output.set(*i, iterator.depth)?;
                }
            }
            result
        };

        // Safety: the returned value is unused.
        unsafe {
            func.call_unprotected([result])?;
        }

        // // Safety: the returned value is unused.
        // unsafe { func.call_unprotected((result, depth))?; }

        // // 0
        // unsafe { func.call_unprotected([])?; }

        // // 27
        // unsafe { func.call_unprotected((result, depth, func, props))?; }

        // // 13
        // unsafe { func.call_unprotected((result, depth))?; }

        // // 10
        // env.vector((result, depth))?;

        // // 6
        // env.cons(result, depth)?;

        // // 0
        // use emacs::call::IntoLispArgs;
        // (result, depth).into_lisp_args(env)?;

        item = iterator.next();
    }
    // for (_, depth) in iterator {
    //     let result = _current_node(&iterator.cursor.clone(), props, output, env)?;
    //     func.call((result, depth))?;
    // }
    Ok(())
}
