use emacs::{defun, Result, Value, IntoLisp, ResultExt, Vector};
use emacs::failure;

use tree_sitter::{Parser, Point};

use crate::types::{SharedTree, shared, Language, Range};

#[defun(user_ptr)]
fn _parser() -> Result<Parser> {
    Ok(Parser::new())
}

#[defun]
fn _set_language(parser: &mut Parser, language: Language) -> Result<()> {
    parser.set_language(language.into()).map_err(failure::err_msg);
    Ok(())
}

#[defun(user_ptr(direct))]
fn parse(parser: &mut Parser, read: Value, old_tree: Option<&SharedTree>) -> Result<SharedTree> {
    let env = read.env;
    let old_tree = match old_tree {
        Some(v) => Some(v.try_borrow()?),
        _ => None,
    };
    let old_tree = match &old_tree {
        Some(r) => Some(&**r),
        _ => None,
    };
    let input = |byte: usize, position: Point| -> String {
        let fragment = env
            .call(
                "funcall",
                &[
                    read,
                    byte.into_lisp(env).unwrap_or_propagate(),
                    position.row.into_lisp(env).unwrap_or_propagate(),
                    position.column.into_lisp(env).unwrap_or_propagate(),
                ],
            )
            .unwrap_or_propagate();
        fragment.into_rust::<String>().unwrap_or_propagate()
    };
    // TODO: Support error cases (None).
    let tree = parser.parse_buffering_with(input, old_tree).unwrap();
    Ok(shared(tree))
}

#[defun]
fn parse_string(parser: &mut Parser, s: String) -> Result<Option<SharedTree>> {
    let tree = parser.parse(s, None);
    Ok(tree.map(shared))
}

#[defun]
fn _reset(parser: &mut Parser) -> Result<()> {
    Ok(parser.reset())
}

#[defun]
fn _timeout_micros(parser: &mut Parser) -> Result<u64> {
    Ok(parser.timeout_micros())
}

#[defun]
fn _set_timeout_micros(parser: &mut Parser, timeout_micros: u64) -> Result<()> {
    Ok(parser.set_timeout_micros(timeout_micros))
}

#[defun]
fn _set_included_ranges(parser: &mut Parser, ranges: Value) -> Result<()> {
    let ranges = Vector(ranges);
    let len = ranges.size()?;
    let included = &mut Vec::with_capacity(len);
    for i in 0..len {
        let range: Range = ranges.get(i)?;
        included.push(range.into());
    }
    Ok(parser.set_included_ranges(included))
}
