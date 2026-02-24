use std::env;
use std::fs;
use std::io::Write;
use std::path::Path;

fn main() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let programs_dir = Path::new(&manifest_dir).join("programs");

    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("generated_tests.rs");
    let mut output = fs::File::create(dest_path).unwrap();

    generate_tests(&mut output, &programs_dir, &programs_dir);

    println!("cargo:rerun-if-changed=programs");
}

fn generate_tests(output: &mut impl Write, base: &Path, dir: &Path) {
    let mut entries: Vec<_> = fs::read_dir(dir)
        .unwrap()
        .filter_map(|e| e.ok())
        .collect();
    entries.sort_by_key(|e| e.path());

    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            generate_tests(output, base, &path);
        } else if path.extension().map_or(false, |e| e == "ndct") {
            let relative = path.strip_prefix(base).unwrap();
            let stem = relative
                .with_extension("")
                .to_string_lossy()
                .replace(['/', '\\', '-', ' '], "_");
            let test_name = format!("test_{stem}");
            let path_str = relative.to_string_lossy();

            writeln!(output, "#[test]").unwrap();
            writeln!(output, "fn {test_name}() {{").unwrap();
            writeln!(
                output,
                "    run_test(std::path::Path::new(env!(\"CARGO_MANIFEST_DIR\")).join(\"programs/{path_str}\")).expect(\"test failed\");",
            )
            .unwrap();
            writeln!(output, "}}").unwrap();
        }
    }
}
