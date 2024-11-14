use std::{fs::{File}, path::Path};

pub fn compile_file(path: impl AsRef<Path>) {
    let file = File::open(path);
}