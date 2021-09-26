// Change case of "this text" repeatedly.
unsafe {
    input_function.call_unprotected((bytepos, point.line_number(), point.byte_column()))
        .and_then(|v| v.into_rust())
}
