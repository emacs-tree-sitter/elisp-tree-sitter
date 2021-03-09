emacs::define_errors! {
    tsc_error "Tree-sitter core error"

    tsc_lang_load_failed "Language load failed" (tsc_error)
    tsc_lang_abi_error "Language's ABI is incompatible" (tsc_error)
    tsc_lang_abi_too_old "Language's ABI is too old" (tsc_lang_load_failed tsc_lang_abi_error)
    tsc_lang_abi_too_new "Language's ABI is too new" (tsc_lang_load_failed tsc_lang_abi_error)

    tsc_invalid_ranges "Invalid parsing ranges" (tsc_error)

    tsc_query_invalid "Invalid query" (tsc_error)
    tsc_query_invalid_syntax "Query syntax error" (tsc_query_invalid)
    tsc_query_invalid_node_type "Query contains invalid node type" (tsc_query_invalid)
    tsc_query_invalid_field "Query contains invalid field name" (tsc_query_invalid)
    tsc_query_invalid_capture "Query contains undeclared capture name" (tsc_query_invalid)
    tsc_query_invalid_predicate "Query contains invalid predicate usage" (tsc_query_invalid)
    tsc_query_invalid_structure "Query contains invalid pattern structure" (tsc_query_invalid)
}
