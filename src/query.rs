use std::cell::RefCell;

use emacs::{defun, Result, Value, Vector, Error, Env, IntoLisp};

use tree_sitter::{Query, QueryCursor, Node};

use crate::types::*;

fn vec_to_vector<'e, T: IntoLisp<'e>>(env: &'e Env, vec: Vec<T>) -> Result<Vector<'e>> {
    let vector = env.make_vector(vec.len(), ())?;
    for (i, v) in vec.into_iter().enumerate() {
        vector.set(i, v)?;
    }
    Ok(vector)
}

// -------------------------------------------------------------------------------------------------
// Query

/// Create a new query from a SOURCE containing one or more S-expression patterns.
///
/// The query is associated with LANGUAGE, and can only be run on syntax nodes
/// parsed with LANGUAGE.
#[defun(user_ptr)]
fn _make_query(language: Language, source: String) -> Result<Query> {
    // TODO: Better error message
    Ok(Query::new(language.into(), &source).unwrap())
}

macro_rules! defun_query_methods {
    ($($(#[$meta:meta])* $($lisp_name:literal)? fn $name:ident $( ( $( $param:ident : $type:ty ),* ) )? -> $rtype:ty $(; $into:ident)? )*) => {
        $(
            #[defun$((name = $lisp_name))?]
            $(#[$meta])*
            fn $name(query: &Query, $( $( $param : $type ),* )? ) -> Result<$rtype> {
                Ok(query.$name( $( $( $param ),* )? )$(.$into())?)
            }
        )*
    };
}

defun_query_methods! {
    /// Return the byte position where the NTH pattern starts in QUERY's source.
    "query-start-byte-for-pattern" fn start_byte_for_pattern(nth: usize) -> BytePos; into

    /// Return the number of patterns in QUERY.
    "query-count-patterns" fn pattern_count -> usize
}

/// Return the names of the captures used in QUERY.
#[defun(mod_in_name = true)]
fn capture_names(query: Value) -> Result<Vector> {
    let env = query.env;
    let query = query.into_ref::<Query>()?;
    let names = query.capture_names();
    let vec = env.make_vector(names.len(), ())?;
    for (i, name) in names.iter().enumerate() {
        vec.set(i, name)?;
    }
    Ok(vec)
}

/// Disable a certain capture within QUERY, by specifying its NAME.
///
/// This prevents the capture from being returned in matches, and also avoids any
/// resource usage associated with recording the capture.
#[defun]
fn _disable_capture(query: &mut Query, name: String) -> Result<()> {
    query.disable_capture(&name);
    Ok(())
}

// -------------------------------------------------------------------------------------------------
// QueryCursor

/// Create a new cursor for executing a given query.
///
/// The cursor stores the state that is needed to iteratively search for matches.
#[defun(user_ptr)]
fn make_query_cursor() -> Result<QueryCursor> {
    Ok(QueryCursor::new())
}

fn text_callback<'e>(
    node: &'e RNode,
    text_callback: Value<'e>,
    error: &'e RefCell<Option<Error>>,
) -> impl FnMut(Node<'e>) -> String + 'e {
    move |child| {
        let child = node.map(|_| child);
        text_callback.call((child,)).and_then(|v| v.into_rust()).unwrap_or_else(|e| {
            error.borrow_mut().replace(e);
            "".to_owned()
        })
    }
}

#[defun]
fn _query_cursor_matches<'e>(
    cursor: &mut QueryCursor,
    query: &Query,
    node: &RNode,
    index_only: Option<Value<'e>>,
    text_function: Value<'e>,
) -> Result<Vector<'e>> {
    let error = RefCell::new(None);
    let matches = cursor.matches(
        query,
        node.borrow().clone(),
        text_callback(node, text_function, &error),
    );
    let mut vec = vec![];
    let env = text_function.env;
    let capture_names = query.capture_names();
    for m in matches {
        if let Some(error) = error.borrow_mut().take() {
            return Err(error);
        }
        let captures = env.make_vector(m.captures.len(), ())?;
        for (ci, c) in m.captures.iter().enumerate() {
            let captured_node = node.map(|_| c.node);
            let capture = if index_only.is_some() {
                env.cons(c.index, captured_node)?
            } else {
                env.cons(&capture_names[c.index as usize], captured_node)?
            };
            captures.set(ci, capture)?;
        }
        let _match = env.cons(m.pattern_index, captures)?;
        vec.push(_match);
    }
    vec_to_vector(env, vec)
}

#[defun]
fn _query_cursor_captures<'e>(
    cursor: &mut QueryCursor,
    query: &Query,
    node: &RNode,
    index_only: Option<Value<'e>>,
    text_function: Value<'e>,
) -> Result<Vector<'e>> {
    let error = RefCell::new(None);
    let captures = cursor.captures(
        query,
        node.borrow().clone(),
        text_callback(node, text_function, &error),
    );
    let mut vec = vec![];
    let env = text_function.env;
    let capture_names = query.capture_names();
    for (m, capture_index) in captures {
        if let Some(error) = error.borrow_mut().take() {
            return Err(error);
        }
        let c = m.captures[capture_index];
        let captured_node = node.map(|_| c.node);
        let capture = if index_only.is_some() {
            env.cons(c.index, captured_node)?
        } else {
            env.cons(&capture_names[c.index as usize], captured_node)?
        };
        vec.push(capture);
    }
    vec_to_vector(env, vec)
}

/// Limit CURSOR's query executions to the range of byte positions, from BEG to END.
#[defun]
fn set_byte_range(cursor: &mut QueryCursor, beg: BytePos, end: BytePos) -> Result<()> {
    cursor.set_byte_range(beg.into(), end.into());
    Ok(())
}

/// Limit CURSOR's query executions to the point range, from BEG to END.
///
/// A "point" in this context is a (LINE-NUMBER . BYTE-COLUMN) pair. See
/// `ts-parse-chunks' for a more detailed explanation.
#[defun]
fn set_point_range(cursor: &mut QueryCursor, beg: Point, end: Point) -> Result<()> {
    cursor.set_point_range(beg.into(), end.into());
    Ok(())
}
