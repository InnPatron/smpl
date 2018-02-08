use std::collections::HashMap;

use super::ASMBackend;
use ast::Ident;
use analysis::metadata::*;
use analysis::smpl_type::StructType;
use analysis::Universe;

#[derive(Debug)]
pub enum Layout {
    FlatData(DataLayout),
    Struct(StructLayout),
}

impl Layout {
    pub fn total_size(&self) -> usize {
        match *self {
            Layout::FlatData(ref l) => l.total_size(),
            Layout::Struct(ref l) => l.layout().total_size(),
        }
    }
}

#[derive(Debug)]
pub struct DataLayout {
    data_size: usize,
    padding: usize,
}

impl DataLayout {
    pub fn new(size: usize, alignment: usize) -> DataLayout {
        let data_size = size;
        let padding = data_size % alignment;

        DataLayout {
            data_size: data_size,
            padding: padding
        }
    }

    pub fn total_size(&self) -> usize {
        self.data_size + self.padding
    }

    pub fn data_size(&self) -> usize {
        self.data_size
    }

    pub fn padding(&self) -> usize {
        self.padding
    }
}

#[derive(Debug)]
pub struct StructLayout {
    fields: HashMap<Ident, (usize, DataLayout)>,        // Offset
    data_layout: DataLayout,
}

impl StructLayout {
    pub fn new(backend: &ASMBackend, universe: &Universe, 
               struct_t: &StructType, meta: &StructMetadata) -> StructLayout {

        let mut fields = HashMap::new();

        let mut total_size = 0;
        let mut current_offset = 0;
        for field in meta.order() {
            let field_type = *(struct_t.fields.get(field).unwrap());
            let field_size = backend.layout(field_type).total_size();

            let data_layout = DataLayout::new(field_size, backend.byte_alignment());
            let total_field_size = data_layout.total_size();

            fields.insert(field.clone(), (current_offset, data_layout));

            current_offset += total_field_size;
            total_size += total_field_size;
        }

        let struct_layout = DataLayout::new(total_size, backend.byte_alignment());
        
        StructLayout {
            fields: fields,
            data_layout: struct_layout,
        }
    }

    pub fn layout(&self) -> &DataLayout {
        &self.data_layout
    }

    pub fn fields(&self) -> &HashMap<Ident, (usize, DataLayout)> {
        &self.fields
    }
}
