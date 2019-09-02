use std::{fs, borrow::Cow};

use tree_sitter::{NodeSource, Point, PropertySheet};
use tree_sitter_highlight::{load_property_sheet, Highlighter, HighlightEvent, Properties};

use emacs::{defun, Value, Result};
use emacs::failure;
use crate::types::Language;
use crate::tree::Tree;

#[derive(Clone)]
pub struct BufferSource<'e> {
    read_function: Value<'e>,
    get_bytes_function: Value<'e>,
    max_len_function: Value<'e>,
}

fn cow<'a>(text: String) -> Cow<'a, [u8]> {
    Cow::Owned(text.into_bytes())
}

impl<'e> NodeSource<'e> for BufferSource<'e> {
    fn bytes(&self, start: usize, end: usize) -> Cow<'e, [u8]> {
        self.get_bytes_function
            .call((start, end))
            .and_then(|v| v.into_rust())
            .map(cow)
            .unwrap_or_else(|e| {
                panic!("Unable to get bytes {}:{}, error: {}", start, end, e)
            })
    }

    fn max_len(&self) -> usize {
        self.max_len_function
            .call([])
            .and_then(|v| v.into_rust())
            .unwrap_or_else(|e| panic!("Unable to get buffer's max_len, error: {:#?}", e))
    }

    fn read(&self, byte_offset: usize, point: Point) -> Cow<'e, [u8]> {
        self.read_function
            .call((byte_offset, point.row, point.column))
            .and_then(|v| v.into_rust())
            .map(cow)
            .unwrap_or_else(|e| {
                panic!("Unable to get bytes from offset {}, error: {}", byte_offset, e)
            })
    }
}

#[defun(user_ptr, mod_in_name = true)]
fn _load_property_sheet(language: Language, file: String) -> Result<PropertySheet<Properties>> {
    load_property_sheet(language.into(), &fs::read_to_string(file)?)
        .map_err(failure::err_msg)
}

#[allow(unused)]
#[defun(mod_in_name = true)]
fn print_events(
    tree: Tree,
    property_sheet: &PropertySheet<Properties>,
    read_function: Value,
    get_bytes_function: Value,
    max_len_function: Value,
) -> Result<()> {
    let source = BufferSource { read_function, get_bytes_function, max_len_function };
    let tree = tree.borrow().clone();
    let highlighter = Highlighter::new_with_tree(source, tree, &property_sheet, |_| None, None)
        .map_err(failure::err_msg)?;
    for maybe_event in highlighter {
        let event = maybe_event.map_err(failure::err_msg)?;
//        match event {
//            HighlightEvent::Source(s, range) => {
//                eprintln!("hl   {} {:#?}", s, range);
//            }
//            HighlightEvent::HighlightStart(t) => {
//                eprintln!("hl {:#?}", t);
//            }
//            HighlightEvent::HighlightEnd => {
//                eprintln!("hl ~~~~~~~~~~~~~~~~~~~~~~~");
//            }
//        }
    }
    Ok(())
}
