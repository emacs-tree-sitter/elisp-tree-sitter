use std::mem;

use emacs::{defun, Result};

use libloading::{Library, Symbol};

use crate::types::Language;

/// Load the shared lib FILE and return the language under SYMBOL-NAME.
#[defun]
fn _load_language(file: String, symbol_name: String) -> Result<Language> {
    let lib = Library::new(file)?;
    let tree_sitter_lang: Symbol<'_, unsafe extern "C" fn() -> _> =
        unsafe { lib.get(symbol_name.as_bytes())? };
    let language: Language = unsafe { tree_sitter_lang() };
    // Avoid segmentation fault by not unloading the lib, as language is a static piece of data.
    mem::forget(lib);
    Ok(language)
}

/// Return the number of distinct node types defined in LANG.
#[defun]
fn count_types(lang: Language) -> Result<usize> {
    Ok(lang.0.node_kind_count())
}

/// Return a LANG's node type string, given its numerical ID.
#[defun]
fn type_from_id(lang: Language, id: u16) -> Result<&'static str> {
    Ok(lang.0.node_kind_for_id(id))
}

/// Return t if the numeric ID identifies a named node type in LANG.
#[defun]
fn type_named_p(lang: Language, id: u16) -> Result<bool> {
    Ok(lang.0.node_kind_is_named(id))
}

/// Return the numerical id of FIELD-NAME in LANG, nil if FIELD-NAME is invalid.
#[defun]
fn field_id(lang: Language, field_name: String) -> Result<Option<u16>> {
    Ok(lang.0.field_id_for_name(field_name))
}
