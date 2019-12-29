use super::metadata::Metadata;
use super::mod_resolver::AnalyzableRawProgram;

#[derive(Debug)]
pub struct MetadataError;

pub(super) fn collect_metadata(metadata: &mut Metadata, raw_program: &AnalyzableRawProgram)
    -> Result<(), MetadataError> {

    module_metadata(metadata, raw_program);


    Ok(())
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
