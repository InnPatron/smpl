macro_rules! wf_error {
    ($kind: expr, $span: expr) => {
        Err(WfError {
            error: $kind,
            span: $span,
        })
    };
}
