extern crate lalrpop;

use std::fs;

fn main() {
    // somehow, parser.rs gets readonly flag on Windows.
    if let Ok(meta) = fs::metadata("src/parser.rs") {
        let mut perms = meta.permissions();
        perms.set_readonly(false);
        fs::set_permissions("src/parser.rs", perms).unwrap();
    }

    lalrpop::process_root().unwrap();
}
