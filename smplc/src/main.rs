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
    let input_val = matches.value_of("INPUT").unwrap();
    let backend_val = matches.value_of("BACKEND").unwrap();
    let output_val = matches.value_of("OUTPUT").unwrap();

    let input_path = Path::new(input_val);
    let mut file = match File::open(input_path.clone()) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Cannot open input file ({}):\n{}",
                      input_path.display(), err);
            return;
        }
    };

    let mut input = String::new();
    match file.read_to_string(&mut input) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Failed to read input file ({}) contents:\n{}", 
                      input_path.display(), err);
            return;
        }
    }

    let backend = match Backend::match_str(backend_val) {
        Some(backend) => backend,
        None => {
            eprintln!("Unknown backend '{}'.", backend_val);
            return;
        }
    };

    let output_dir = Path::new(output_val);
    if output_dir.is_dir() == false {
        eprintln!("Output path '{}' should be a directory", output_val);
        return;
    }

    let mut output_path = output_dir.to_path_buf();
    output_path.push(input_path.file_stem().unwrap());
    output_path.set_extension("rs");

    let mut output_file = {
        let open_result = OpenOptions::new()
                                        .append(false)
                                        .create_new(true)
                                        .open(output_path.clone());
        match open_result {
            Ok(file) => file,
            Err(err) => {
                eprintln!("Failed to open output file ({}):\n{}",
                          output_path.display(), err);
                return;
            }
        }
    };

    let result = match backend {
        Backend::Rust => rust_gen(&input),
    };

    let output = match result {
        Ok(output) => output,
        Err(err) => {
            eprintln!("Compilation error: {}", err);
            return;
        }
    };

    if let Err(err) = output_file.write_all(output.as_bytes()) {
        eprintln!("Failed to write to output file ({}):\n{}", 
                  output_path.display(), err);
        return;
    }
}

fn rust_gen(input: &str) -> Result<String, String> {
    use smpl::*;

    let ast = parse_program(input).map_err(|err| format!("{:?}", err))?;
    let program = check_ast(ast).map_err(|err| format!("{:?}", err))?;

    let mut gen = RustCodeGenerator::new();
    gen.emit_program(&program);

    Ok(gen.program().to_string())
}

enum Backend {
    Rust
}

impl Backend {
    fn match_str(str: &str) -> Option<Backend> {
        match str {
            "0" => Some(Backend::Rust),
            _ => None,
        }
    }
}
