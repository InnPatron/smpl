#[macro_use]
extern crate clap;
extern crate smpl;

use clap::App;

use std::fs::{File, OpenOptions};
use std::path::Path;
use std::io::{Read, Write};

fn main() {
    let yaml = load_yaml!("cli.yaml");
    let matches = App::from_yaml(yaml).get_matches();

    // Required
    let input_vals: Vec<_> = matches.values_of("INPUT").unwrap().collect();
    let backend_val = matches.value_of("BACKEND").unwrap();
    let output_val = matches.value_of("OUTPUT").unwrap();

    let input_path_list: Vec<_> = input_vals.iter().map(|input| Path::new(input)).collect();
    let mut files = Vec::new();
    for path in input_path_list.iter() {
        let mut file = match File::open(path.clone()) {
            Ok(file) => files.push(file),
            Err(err) => {
                eprintln!(
                    "Cannot open input file ({}):\n{}",
                    path.display(),
                    err
                );
                return;
            }
        };
    }

    let mut file_inputs = Vec::new();
    for (ref mut file, ref path) in files.iter_mut().zip(input_path_list.iter()) {
        let mut input = String::new();
        match file.read_to_string(&mut input) {
            Ok(_) => file_inputs.push(input),
            Err(err) => {
                eprintln!(
                    "Failed to read input file ({}) contents:\n{}",
                    path.display(),
                    err
                );
                return;
            }
        }
    }

    let backend = match Backend::match_str(backend_val) {
        Some(backend) => backend,
        None => {
            eprintln!("Unknown backend '{}'.", backend_val);
            return;
        }
    };

    let mut output_path = Path::new(output_val).to_path_buf();
    if output_path.is_dir() {
        // Default output name
        output_path.push("output");
        output_path.set_extension("rs");
    }
    
    
    let mut output_file = {
        let open_result = OpenOptions::new()
            .write(true)
            .append(false)
            .create(true)
            .open(output_path.clone());
        match open_result {
            Ok(file) => file,
            Err(err) => {
                eprintln!(
                    "Failed to open output file ({}):\n{}",
                    output_path.display(),
                    err
                );
                return;
            }
        }
    };

    let mut input = Vec::new();
    for (file_path, code) in input_path_list.iter().zip(file_inputs.iter()) {
        let file_name = file_path.file_stem().map(|s| s.to_str()).unwrap();
        let file_name = match file_name {
            Some(name) => name,
            None => {
                eprintln!(
                    "Path to file {} is not a valid UTF8 String",
                    file_path.display());
                return;
            }
        };

        input.push((file_name, code.as_str()));
    }
           
    let result = match backend {
        Backend::Rust => rust_gen(input),
    };

    let output = match result {
        Ok(output) => output,
        Err(err) => {
            eprintln!("Compilation error: {}", err);
            return;
        }
    };

    if let Err(err) = output_file.set_len(0) {
        eprintln!(
            "Failed to erase output file ({}):\n{}",
            output_path.display(),
            err
        );
        return;
    }

    if let Err(err) = output_file.write_all(&output) {
        eprintln!(
            "Failed to write to output file ({}):\n{}",
            output_path.display(),
            err
        );
        return;
    }
}

fn rust_gen(input: Vec<(&str, &str)>) -> Result<Vec<u8>, String> {
    use smpl::*;

    let mut modules = Vec::new();
    for (file_name, code) in input {

        let mut module = parse_module(code).map_err(|err| format!("{:?}", err))?;
    
        if module.name().is_none() {
            module.set_name(file_name);
        }

        modules.push(module);
    }
    
    let program = check_program(modules).map_err(|err| format!("{:?}", err))?;

    let program = RustBackend::new()
        .wrap_mod()
        .generate(&program);

    let program = match program {
        Ok(p) => p,

        Err(e) => panic!("{:?}", e),
    };

    let mut result = Vec::new();
    if let Some(m) = program.main() {
        result.extend(m.as_bytes());
    }

    let mut mods = program.finalize();

    for m in mods {
        result.extend(m.2.into_bytes());
    }

    Ok(result)
}

enum Backend {
    Rust,
}

impl Backend {
    fn match_str(str: &str) -> Option<Backend> {
        match str {
            "0" => Some(Backend::Rust),
            _ => None,
        }
    }
}
