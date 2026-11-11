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

    let manual_dir = Path::new(&manifest_dir).join("../../manual/src");
    let dest_path = Path::new(&out_dir).join("manual_tests.rs");
    let mut output = fs::File::create(dest_path).unwrap();
    generate_manual_tests(&mut output, &manual_dir, &manual_dir);

    println!("cargo:rerun-if-changed=programs");
    println!("cargo:rerun-if-changed=../../manual/src");
}

/// One test per `ndc` code block in the manual that isn't marked `norun`,
/// named after the file and the line of its opening fence:
/// `manual_reference_types_list_md_5` runs the block at `reference/types/list.md:5`.
fn generate_manual_tests(output: &mut impl Write, base: &Path, dir: &Path) {
    let mut entries: Vec<_> = fs::read_dir(dir).unwrap().filter_map(|e| e.ok()).collect();
    entries.sort_by_key(|e| e.path());

    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            generate_manual_tests(output, base, &path);
            continue;
        }
        if path.extension().is_none_or(|e| e != "md") {
            continue;
        }
        let relative = path
            .strip_prefix(base)
            .unwrap()
            .to_string_lossy()
            .into_owned();
        let markdown = fs::read_to_string(&path).unwrap();
        let mut in_block = false;
        for (index, line) in markdown.lines().enumerate() {
            if !line.starts_with("```") {
                continue;
            }
            if in_block {
                in_block = false;
                continue;
            }
            in_block = true;
            let Some(info) = line.strip_prefix("```ndc") else {
                continue;
            };
            if info.split(',').any(|flag| flag.trim() == "norun") {
                continue;
            }
            let line_number = index + 1;
            let name = relative.replace(['/', '\\', '-', '.', ' '], "_");
            writeln!(output, "#[test]").unwrap();
            writeln!(output, "fn manual_{name}_{line_number}() {{").unwrap();
            writeln!(
                output,
                "    run_manual_example({relative:?}, {line_number});"
            )
            .unwrap();
            writeln!(output, "}}").unwrap();
        }
    }
}

fn generate_tests(output: &mut impl Write, base: &Path, dir: &Path) {
    let mut entries: Vec<_> = fs::read_dir(dir).unwrap().filter_map(|e| e.ok()).collect();
    entries.sort_by_key(|e| e.path());

    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            generate_tests(output, base, &path);
        } else if path.extension().is_some_and(|e| e == "ndc") {
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
                "    run_ndc_test(&Path::new(env!(\"CARGO_MANIFEST_DIR\")).join(\"programs/{path_str}\")).expect(\"test failed\");",
            )
            .unwrap();
            writeln!(output, "}}").unwrap();
        }
    }
}
