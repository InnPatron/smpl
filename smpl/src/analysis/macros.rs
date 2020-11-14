macro_rules! wf_error {
    ($kind: expr, $span: expr) => {
        WfError {
            error: $kind,
            span: $span,
        }
    };
}
