use std::mem;

use emacs::{defun, Result};

use libloading::{Library, Symbol};

use crate::types::Language;

/// Load the shared lib FILE and return the language NAME it contains.
///
/// It is assumed that language's symbol in the shared lib is prefixed with
/// SYMBOL-PREFIX. If SYMBOL-PREFIX is nil, it is assumed to be "tree_sitter_".
///
/// If FILE is nil, load from "~/.tree-sitter/bin/NAME.so". This is where the
/// tree-sitter CLI tool stores the generated shared libs.
///
#[defun]
fn _load_language(name: String, file: Option<String>, symbol_prefix: Option<String>) -> Result<Language> {
    let filename = file.unwrap_or_else(|| {
        // TODO: Get home directory properly.
        format!("{}/.tree-sitter/bin/{}.so", std::env::var("HOME").unwrap(), name)
    });
    let prefix = match &symbol_prefix {
        None => "tree_sitter_",
        Some(s) => s
    };
    let symbol_name = format!("{}{}", prefix, name);
    let lib = Library::new(filename)?;
    let tree_sitter_lang: Symbol<'_, unsafe extern "C" fn() -> _> =
        unsafe { lib.get(symbol_name.as_bytes())? };
    let language: Language = unsafe { tree_sitter_lang() };
    // Avoid segmentation fault by not unloading the lib, as language is a static piece of data.
    mem::forget(lib);
    Ok(language)
}

#[defun]
fn count_types(lang: Language) -> Result<usize> {
    Ok(lang.0.node_kind_count())
}

#[defun]
fn type_from_id(lang: Language, id: u16) -> Result<&'static str> {
    Ok(lang.0.node_kind_for_id(id))
}

#[defun]
fn type_named_p(lang: Language, id: u16) -> Result<bool> {
    Ok(lang.0.node_kind_is_named(id))
}

#[defun]
fn field_id(lang: Language, field_name: String) -> Result<Option<u16>> {
    Ok(lang.0.field_id_for_name(field_name))
}
