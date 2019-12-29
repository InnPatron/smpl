use super::semantic_data::Universe;
use super::metadata::Metadata;
use super::mod_resolver::AnalyzableRawProgram;

#[derive(Debug)]
pub struct MetadataError;

pub(super) fn collect_metadata(metadata: &mut Metadata, raw_program: &AnalyzableRawProgram)
    -> Result<(), MetadataError> {

    module_metadata(metadata, raw_program);


    Ok(())
}

pub(super) fn collect_metadata_post(metadata: &mut Metadata, universe: &Universe)
    -> Result<(), MetadataError> {

    smpl_function_parameters(metadata, universe);

    Ok(())
}

///
/// Collects parameter order (name, local variable ID) for SMPL functions.
///
/// Parameter order of builtin functions are NOT collected.
///
fn smpl_function_parameters(metadata: &mut Metadata, universe: &Universe) {
    use super::semantic_data::{Function, SMPLFunction, AnonymousFn};
    use super::metadata::FunctionParameter;

    for (fn_id, func) in universe.all_fns() {
        match func {
            Function::SMPL(SMPLFunction {
                ref analysis_context,
                ..
            }) | Function::Anonymous(AnonymousFn {
                ref analysis_context,
                ..
            }) => {
                metadata.insert_function_param_ids(
                    fn_id,
                    analysis_context.param_order()
                        .iter()
                        .map(|(ident, id)| FunctionParameter::new(ident.clone(), id.clone()))
                        .collect()
                );
            }

            _ => (),
        }
    }
}


///
/// Map module name to ModuleId.
/// Required by SMPL consumers for better name diagnostics.
fn module_metadata(metadata: &mut Metadata, raw_program: &AnalyzableRawProgram) {

    for (mod_id, module_data) in raw_program.module_map.iter() {
        let name = module_data.name.data().clone();
        metadata
            .mod_metadata_mut()
            .map_module(name, mod_id.clone());
    }

}
