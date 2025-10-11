use std::{fs::{File}, path::Path};
pub fn compile_src(path_to_src: impl AsRef<Path>) {
    let _src = File::open(path_to_src).unwrap();
}