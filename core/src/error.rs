emacs::define_errors! {
    tsc_error "Tree-sitter core error"

    tsc_lang_load_failed "Language load failed" (tsc_error)
    tsc_lang_abi_error "Language's ABI is incompatible" (tsc_error)
    tsc_lang_abi_too_old "Language's ABI is too old" (tsc_lang_load_failed tsc_lang_abi_error)
    tsc_lang_abi_too_new "Language's ABI is too new" (tsc_lang_load_failed tsc_lang_abi_error)
}
