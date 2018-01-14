#[macro_use]
extern crate clap;
extern crate smpl;

use clap::App;

fn main() {
    let yaml = load_yaml!("cli.yaml");
    let matches = App::from_yaml(yaml).get_matches();
}
