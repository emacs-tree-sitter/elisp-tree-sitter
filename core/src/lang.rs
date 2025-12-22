use std::{collections::HashMap, mem, os, sync::Mutex};

use emacs::{defun, Result, ResultExt, GlobalRef, Value, Env, IntoLisp, FromLisp, ErrorKind};

use libloading::{Library, Symbol};
use once_cell::sync::Lazy;

use crate::{error, types};
use tree_sitter::{LANGUAGE_VERSION, MIN_COMPATIBLE_LANGUAGE_VERSION};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
#[repr(transparent)]
pub struct Language(pub(crate) tree_sitter::Language);

impl IntoLisp<'_> for Language {
    fn into_lisp(self, env: &Env) -> Result<Value> {
        // Safety: Language has the same representation as the opaque pointer type.
        let ptr: *mut os::raw::c_void = unsafe { mem::transmute(self) };
        // Safety: The finalizer does nothing.
        unsafe { env.make_user_ptr(Some(no_op::<Language>), ptr) }
    }
}

impl FromLisp<'_> for Language {
    fn from_lisp(value: Value) -> Result<Language> {
        match value.get_user_finalizer()? {
            Some(fin) if fin == no_op::<Language> => {
                let ptr = value.get_user_ptr()?;
                // Safety: Language has the same representation as the opaque pointer type.
                Ok(unsafe { mem::transmute(ptr) })
            }
            _ => Err(ErrorKind::WrongTypeUserPtr { expected: "TreeSitterLanguage" }.into())
        }
    }
}

impl_newtype_traits!(Language);

impl_pred!(language_p, Language);

impl Language {
    pub fn id(self) -> usize {
        unsafe { mem::transmute(self) }
    }

    pub fn info(self) -> &'static LangInfo {
        // TODO: Explain the safety.
        LANG_INFOS.try_lock().expect("Failed to access language info registry")
            .get(&self.id())
            .map(|info| unsafe { types::erase_lifetime(info) })
            .expect("Failed to get language info from the registry")
    }
}

unsafe extern "C" fn no_op<T>(_: *mut os::raw::c_void) {}

// -------------------------------------------------------------------------------------------------

pub struct LangInfo {
    load_file: String,
    lang_symbol: GlobalRef,
    _lib: Library,
    node_types: Vec<GlobalRef>,
    field_names: Vec<GlobalRef>,
}

impl LangInfo {
    #[inline]
    pub(crate) fn node_type(&self, id: u16) -> Option<&GlobalRef> {
        self.node_types.get(id as usize)
    }

    #[inline]
    pub(crate) fn field_name(&self, id: u16) -> Option<&GlobalRef> {
        if id == 0 {
            None
        } else {
            self.field_names.get(id as usize - 1)
        }
    }
}

// TODO: Consider optimizing for accessing language's metadata, i.e. making Language a big wrapper
// around tree_sitter::Language, so that hash lookup happens only when returning the language of a
// parser/tree/node/query.
static LANG_INFOS: Lazy<Mutex<HashMap<usize, LangInfo>>> = Lazy::new(|| Mutex::new(HashMap::new()));

/// Load the shared lib FILE and return the language under SYMBOL-NAME.
/// The language's name symbol is set to LANG-SYMBOL.
#[defun]
fn _load_language(file: String, symbol_name: String, lang_symbol: Value) -> Result<Language> {
    let env = lang_symbol.env;
    let lib = unsafe { Library::new(&file) }.or_signal(env, error::tsc_lang_load_failed)?;
    let tree_sitter_lang: Symbol<'_, unsafe extern "C" fn() -> _> =
        unsafe { lib.get(symbol_name.as_bytes()) }.or_signal(env, error::tsc_lang_load_failed)?;
    let language: tree_sitter::Language = unsafe { tree_sitter_lang() };
    let version = language.abi_version();
    if version < MIN_COMPATIBLE_LANGUAGE_VERSION {
        return env.signal(error::tsc_lang_abi_too_old, (
            version, supported_abi_range(env)?, file
        ));
    }
    if version > LANGUAGE_VERSION {
        return env.signal(error::tsc_lang_abi_too_new, (
            version, supported_abi_range(env)?, file
        ));
    }
    let node_types = (0..language.node_kind_count() as u16).map(|id| {
        let type_str = language.node_kind_for_id(id).expect("Failed to get node type for id");
        let value = if !language.node_kind_is_visible(id) {
            env.intern(&format!(":{}", type_str)).expect("Failed to intern keyword for node type")
        } else if language.node_kind_is_named(id) {
            env.intern(type_str).expect("Failed to intern symbol for node type")
        } else {
            type_str.into_lisp(env).expect("Failed to make string for node type")
        };
        value.make_global_ref()
    }).collect();
    let field_names = (1..=language.field_count() as u16).map(|id| {
        let field_str = language.field_name_for_id(id).expect("Failed to get field name for id");
        env.intern(&format!(":{}", field_str)).expect("Failed to intern keyword for field name")
            .make_global_ref()
    }).collect();
    let language: Language = language.into();
    let id = language.clone().id().clone();
    LANG_INFOS.try_lock().expect("Failed to access language info registry")
        .insert(id, LangInfo {
            load_file: file,
            lang_symbol: lang_symbol.make_global_ref(),
            _lib: lib,
            node_types,
            field_names,
        });
    Ok(language)
}

/// Return LANGUAGE's name, as a symbol.
#[defun]
fn _lang_symbol(language: Language) -> Result<&'static GlobalRef> {
    Ok(&language.info().lang_symbol)
}

/// Return the shared lib file that LANGUAGE was loaded from.
#[defun]
fn _lang_load_file(language: Language) -> Result<&'static String> {
    Ok(&language.info().load_file)
}

/// Return the node type associated with the numeric TYPE-ID in LANGUAGE.
///
/// For named nodes, the node type is a symbol. For example: 'identifier, 'block.
/// For anonymous nodes, the node type is a string. For example: "if", "else".
/// For auxiliary (invisible) nodes, the node type is a keyword. For example: :end, :_expression.
#[defun]
fn lang_node_type(language: Language, type_id: u16) -> Result<Option<&'static GlobalRef>> {
    Ok(language.info().node_type(type_id))
}

/// Return a field's name keyword, given its numeric FIELD-ID in LANGUAGE.
#[defun]
fn lang_field(language: Language, field_id: u16) -> Result<Option<&'static GlobalRef>> {
    Ok(language.info().field_name(field_id))
}

/// Return the numeric id of TYPE-NAME in LANGUAGE.
#[defun]
fn _lang_type_id_for_name(language: Language, type_name: String, named: Option<Value>) -> Result<u16> {
    Ok(language.0.id_for_node_kind(&type_name, named.is_some()))
}

/// Return the range of language ABI's that this module can load.
#[defun]
fn supported_abi_range(env: &Env) -> Result<Value> {
    env.cons(MIN_COMPATIBLE_LANGUAGE_VERSION, LANGUAGE_VERSION)
}

macro_rules! defun_lang_methods {
    ($($(#[$meta:meta])* $($lisp_name:literal)? fn $name:ident $( ( $( $param:ident : $type:ty ),* ) )? -> $rtype:ty )*) => {
        $(
            #[defun$((name = $lisp_name))?]
            $(#[$meta])*
            fn $name(language: Language, $( $( $param : $type ),* )? ) -> Result<$rtype> {
                Ok((language.0).$name( $( $( $param ),* )? ))
            }
        )*
    };
}

defun_lang_methods! {
    /// Return the ABI version number for LANGUAGE.
    /// This version number is used to ensure that languages were generated by a
    /// compatible version of tree-sitter. `tsc-set-language' will fail if the language
    /// is incompatible, so there's rarely a need to use this function, except for
    /// debugging purposes.
    "lang-version" fn abi_version -> usize

    /// Return the number of distinct node types defined in LANGUAGE.
    "lang-count-types" fn node_kind_count -> usize

    /// Return the number of distinct field names defined in LANGUAGE.
    "lang-count-fields" fn field_count -> usize

    /// Return t if the numeric TYPE-ID identifies a named node type in LANGUAGE.
    "lang-node-type-named-p" fn node_kind_is_named(type_id: u16) -> bool
}

/// Return the numeric id of FIELD-NAME in LANGUAGE.
#[defun(name = "-lang-field-id-for-name")]
fn field_id_for_name(language: Language, field_name: String) -> Result<Option<u16>> {
    Ok(language
       .0
       .field_id_for_name(&field_name)
       .map(|nz| nz.get()))
}
