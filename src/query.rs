use std::cell::RefCell;

use emacs::{defun, Result, Value, Vector, Error, Env, IntoLisp};

use tree_sitter::{Query, QueryCursor, Node};

use crate::types::*;

fn make_vector(env: &Env, len: usize) -> Result<Vector> {
    env.call("make-vector", (len, ())).map(Vector)
}

fn vec_to_vector<'e, T: IntoLisp<'e>>(env: &'e Env, vec: Vec<T>) -> Result<Vector<'e>> {
    let vector = make_vector(env, vec.len())?;
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
fn make_query(language: Language, source: String) -> Result<Query> {
    // TODO: Better error message
    Ok(Query::new(language.into(), &source).unwrap())
}

macro_rules! defun_query_methods {
    ($($(#[$meta:meta])* $($lisp_name:literal)? fn $name:ident $( ( $( $param:ident : $type:ty ),* ) )? -> $rtype:ty )*) => {
        $(
            #[defun$((name = $lisp_name))?]
            $(#[$meta])*
            fn $name(query: &Query, $( $( $param : $type ),* )? ) -> Result<$rtype> {
                Ok(query.$name( $( $( $param ),* )? ))
            }
        )*
    };
}

defun_query_methods! {
    /// Return the byte offset where the NTH pattern starts in QUERY's source.
    "query-start-byte-for-pattern" fn start_byte_for_pattern(nth: usize) -> usize

    /// Return the number of patterns in QUERY.
    "query-count-patterns" fn pattern_count -> usize
}

/// Return the names of the captures used in QUERY.
#[defun(mod_in_name = true)]
fn capture_names(query: Value) -> Result<Vector> {
    let env = query.env;
    let query = query.into_ref::<Query>()?;
    let names = query.capture_names();
    let len = names.len();
    let vec = make_vector(env, len)?;
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
        let _match = make_vector(env, 2)?;
        let captures = make_vector(env, m.captures.len())?;
        for (ci, c) in m.captures.iter().enumerate() {
            let capture = make_vector(env, 2)?;
            if index_only.is_some() {
                capture.set(0, c.index)
            } else {
                capture.set(0, &capture_names[c.index as usize])
            }?;
            capture.set(1, node.map(|_| c.node))?;
            captures.set(ci, capture)?;
        }
        _match.set(0, m.pattern_index)?;
        _match.set(1, captures)?;
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
        let capture = make_vector(env, 2)?;
        if index_only.is_some() {
            capture.set(0, c.index)?;
        } else {
            capture.set(0, &capture_names[c.index as usize])?;
        }
        capture.set(1, node.map(|_| c.node))?;
        vec.push(capture);
    }
    vec_to_vector(env, vec)
}

/// Set the byte range (BEG, END) in which queries will be executed by CURSOR.
#[defun]
fn set_byte_range(cursor: &mut QueryCursor, beg: usize, end: usize) -> Result<()> {
    cursor.set_byte_range(beg, end);
    Ok(())
}

/// Set the point range (BEG, END) in which queries will be executed by CURSOR.
#[defun]
fn set_point_range(cursor: &mut QueryCursor, beg: Point, end: Point) -> Result<()> {
    cursor.set_point_range(beg.into(), end.into());
    Ok(())
}
