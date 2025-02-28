use std::{fs::{File}, path::Path};
use cranelift_codegen;
pub fn compile_src(path_to_src: impl AsRef<Path>) {
    let src = File::open(path_to_src).unwrap();
}