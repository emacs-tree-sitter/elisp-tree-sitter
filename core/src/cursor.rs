use std::{
    os,
    cell::{Ref, RefCell},
    mem,
    ops::{Deref, DerefMut},
};

use emacs::{defun, Result, Value, GlobalRef, Vector};
use tree_sitter::{Tree, TreeCursor};

use crate::{
    types::{self, Shared, Either, BytePos},
    tree::Borrowed,
    node::{RNode, LispUtils},
    lang::Language,
};

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
    pub fn borrow_mut<'e>(&'e mut self) -> RCursorBorrowMut {
        let reft: Ref<'e, Tree> = self.tree.borrow();
        // XXX: Explain the safety here.
        let cursor: &'e mut _ = unsafe { mem::transmute(&mut self.inner) };
        RCursorBorrowMut { reft, cursor }
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
fn make_cursor<'e>(
    tree_or_node: Either<'e, &'e Shared<Tree>, &'e RefCell<RNode>>,
) -> Result<RCursor> {
    match tree_or_node {
        Either::Left(tree, ..) => {
            Ok(RCursor::new(tree.clone(), |tree| tree.walk()))
        }
        Either::Right(node, ..) => {
            let node = node.borrow();
            Ok(RCursor::new(node.clone_tree(), |_| node.borrow().walk()))
        }
    }
}

/// Return CURSOR's current node.
#[defun]
fn current_node(cursor: &RCursor) -> Result<RNode> {
    Ok(RNode::new(cursor.clone_tree(), |_| cursor.borrow().node()))
}

emacs::use_symbols! {
    nil
    wtf
    iter_end_of_sequence

    _next        => ":next"
    _close       => ":close"

    _field       => ":field"
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
}

use emacs::*;
use emacs::func::{HandleCall, Manage};

fn iterate(env: &CallEnv) -> Result<Value> {
    let iter: &mut DepthFirstIterator = unsafe { mem::transmute(env.data) };
    let control = env.get_arg(0);
    iter_next(iter, control, nil.bind(env))
}

enum TraversalState {
    Start,
    Down,
    Right,
    Done,
}

struct DepthFirstIterator {
    cursor: RCursor,
    state: TraversalState,
    depth: usize,
}

impl DepthFirstIterator {
    fn new(tree: Borrowed<Tree>) -> Self {
        Self {
            cursor: RCursor::new(tree.clone(), |tree| tree.walk()),
            state: Start,
            depth: 0,
        }
    }

    #[inline]
    fn item(&self) -> Option<(RNode, usize)> {
        Some((
            RNode::new(self.cursor.clone_tree(),
                       |_| self.cursor.borrow().node()),
            self.depth,
        ))
    }

    fn close(&mut self) {
        self.state = Done;
    }
}

use TraversalState::*;

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
            Done => None
        }
    }
}

unsafe extern "C" fn iterate_wrapper(
    env: *mut raw::emacs_env,
    nargs: isize,
    args: *mut raw::emacs_value,
    data: *mut os::raw::c_void,
) -> raw::emacs_value {
    let env = Env::new(env);
    let env = CallEnv::new(env, nargs, args, data);
    env.handle_call(iterate)
}

#[defun]
fn generate_depth_first<'e>(tree: Borrowed<'e, Tree>, env: &'e Env) -> Result<Value<'e>> {
    // let tree = tree.borrow();
    let iterator = DepthFirstIterator::new(tree);
    let iterator = Box::new(iterator);
    let iterator = Box::leak(iterator);
    unsafe {
        let iterator = mem::transmute(iterator);
        env.make_function(iterate_wrapper, 1..2, "", iterator)
    }
}

#[defun]
fn iter_next<'e>(iter: &mut DepthFirstIterator, control: Value<'e>, _yield_result: Value) -> Result<Value<'e>> {
    let env = control.env;
    if control.eq(_next.bind(&env)) {
        match iter.next() {
            Some((node, depth)) =>
                env.cons(node, depth),
            None =>
                env.signal(iter_end_of_sequence, []),
        }
    } else if control.eq(_close.bind(&env)) {
        iter.close();
        ().into_lisp(env)
    } else {
        env.signal(wtf, [])
    }
}

#[defun(user_ptr)]
fn _iter(tree: Borrowed<Tree>) -> Result<DepthFirstIterator> {
    Ok(DepthFirstIterator::new(tree))
}

#[defun]
fn _iter_next(iter: &mut DepthFirstIterator) -> Result<bool> {
    Ok(iter.next().is_some())
}

#[defun]
fn _iter_close(iter: &mut DepthFirstIterator) -> Result<()> {
    Ok(iter.close())
}

#[defun]
fn _iter_current_node(iter: &mut DepthFirstIterator, props: Option<Vector>, combined_output: Vector) -> Result<()> {
    let env = combined_output.value().env;
    let cursor = &iter.cursor;
    let output = combined_output.get(0)?;
    let node = _current_node(cursor, props, output, env)?;
    if props.is_none() {
        combined_output.set(0, node)?;
    }
    combined_output.set(1, iter.depth)?;
    // Ok(data)
    // Ok(combined_output)
    Ok(())
}

#[defun]
fn _iter_next_node(iter: &mut DepthFirstIterator, props: Option<Vector>, combined_output: Vector) -> Result<bool> {
    if iter.next().is_some() {
        _iter_current_node(iter, props, combined_output)?;
        Ok(true)
    } else {
        Ok(false)
    }
}

#[defun]
fn _current_node<'e>(cursor: &RCursor, props: Option<Vector<'e>>, output: Option<Vector<'e>>, env: &'e Env) -> Result<Value<'e>> {
    macro_rules! sugar {
        ($prop:ident, $env:ident) => {
            macro_rules! eq {
                ($name:ident) => ($prop.eq($name.bind($env)))
            }
        }
    }
    let node = cursor.borrow().node();
    match props {
        None => RNode::new(cursor.clone_tree(), |_| node).into_lisp(env),
        Some(props) => {
            let result = match output {
                None => env.make_vector(props.len(), ())?,
                Some(output) => output,
            };
            for (i, prop) in props.into_iter().enumerate() {
                sugar!(prop, env);
                if eq!(_type) {
                    result.set(i, node.lisp_type())?;
                } else if eq!(_byte_range) {
                    result.set(i, node.lisp_byte_range(env)?)?;
                } else if eq!(_start_byte) {
                    result.set(i, node.lisp_start_byte())?;
                } else if eq!(_end_byte) {
                    result.set(i, node.lisp_end_byte())?;
                } else if eq!(_field) {
                    result.set(i, current_field(cursor)?)?;
                } else if eq!(_named_p) {
                    result.set(i, node.is_named())?;
                } else if eq!(_extra_p) {
                    result.set(i, node.is_extra())?;
                } else if eq!(_error_p) {
                    result.set(i, node.is_error())?;
                } else if eq!(_missing_p) {
                    result.set(i, node.is_missing())?;
                } else if eq!(_has_error_p) {
                    result.set(i, node.has_error())?;
                } else if eq!(_start_point) {
                    result.set(i, node.lisp_start_point())?;
                } else if eq!(_end_point) {
                    result.set(i, node.lisp_end_point())?;
                } else if eq!(_range) {
                    result.set(i, node.lisp_range())?;
                } else {
                    result.set(i, RNode::new(cursor.clone_tree(), |_| node))?;
                }
            }
            result.into_lisp(env)
        }
    }
}

#[defun]
fn _traverse_depth_first_native(tree: Borrowed<Tree>, func: Value, props: Option<Vector>) -> Result<()> {
    let mut iterator = DepthFirstIterator::new(tree);
    let env = func.env;
    let output = match props {
        None => None,
        Some(props) => Some(env.make_vector(props.len(), ())?),
    };
    // Can't use a for loop because we need to access the cursor to process each item.
    let mut item: Option<(RNode, usize)> = iterator.next();
    while item.is_some() {
        let result = _current_node(&iterator.cursor, props, output, env)?;
        let (_, depth) = item.unwrap();
        // Safety: the returned value is unused.

        unsafe { func.call_unprotected((result, depth))?; }

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

/// Return the field id of CURSOR's current node.
/// Return nil if the current node doesn't have a field.
#[defun]
fn current_field_id(cursor: &RCursor) -> Result<Option<u16>> {
    Ok(cursor.borrow().field_id())
}

/// Return the field associated with CURSOR's current node, as a keyword.
/// Return nil if the current node is not associated with a field.
#[defun]
fn current_field(cursor: &RCursor) -> Result<Option<&'static GlobalRef>> {
    let cursor = cursor.borrow();
    let language: Language = cursor.reft.language().into();
    Ok(cursor.field_id().and_then(|id| language.info().field_name(id)))
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
