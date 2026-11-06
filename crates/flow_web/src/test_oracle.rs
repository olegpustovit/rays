//! Byte-for-byte comparison with the OCaml printers beyond the checked-in goldens: fast WGSL kernels,
//! non-fast WGSL, `js::region` in both precisions, and the fast kernels of the error cases. The
//! expected texts are in fixtures/web/{wgsl,wgsl_fast,wgsl_error,js_region,js_region_fast}, dumped
//! by the OCaml printers (rs/fixtures/web/oracle/dump.ml.txt; see rs/fixtures/web/README.md).
//!
//! A few programs print differently because the Rust `flow_ir` hands the printers a different
//! register program or `kernel_layout::plan` narrowing than OCaml's. Each test lists exactly those
//! cases: the test fails if a listed case starts to match (remove it) or an unlisted one differs.
use crate::js;
use crate::node_harness::repo_path;
use crate::oracle_cases::{error_cases, fast_cases};
use crate::test_program::{carried_fixtures, compile};
use crate::wgsl;
use flow_ir::packed;

fn sanitize(s: &str) -> String { s.chars().map(|c| if c.is_ascii_alphanumeric() || c == '-' { c } else { '_' }).collect() }

fn expected(dir: &str, name: &str, ext: &str) -> String {
    let path = repo_path(&format!("fixtures/web/{dir}/{}{ext}", sanitize(name)));
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()))
}

fn wgsl_text(r: Result<wgsl::Wgsl, rays_diagnostic::Diagnostic>) -> String {
    match r {
        Ok(w) => format!("{}\n{}", w.entry, w.source),
        Err(d) => format!("ERROR {} {}", d.code, d.message),
    }
}

fn region_text(r: Result<String, rays_diagnostic::Diagnostic>) -> String {
    match r {
        Ok(s) => s,
        Err(d) if d.code == "INVALID_ARGUMENT" => format!("EXN Invalid_argument {}", d.message),
        Err(d) => panic!("{}", rays_diagnostic::to_string(&d)),
    }
}

/// Collects the cases whose text differs from the OCaml printer's.
struct Diffs(Vec<String>);

impl Diffs {
    fn same(&mut self, what: &str, name: &str, actual: &str, wanted: &str) {
        if actual != wanted {
            let path = std::env::temp_dir().join(format!("flow_web_actual_{what}_{}.txt", sanitize(name)));
            let _ = std::fs::write(&path, actual);
            self.0.push(format!("{what} {name}"));
        }
    }

    fn region(&mut self, name: &str, program: &packed::T) {
        self.same("js_region", name, &region_text(js::region(false, program)), &expected("js_region", name, ".js"));
        self.same("js_region_fast", name, &region_text(js::region(true, program)), &expected("js_region_fast", name, ".js"));
    }

    /// Fail unless the differing cases are exactly `listed` (kernel_layout narrowing or register
    /// numbering differences of the Rust flow_ir, reported to its owner).
    fn finish(self, listed: &[&str]) {
        let mut actual = self.0;
        actual.sort();
        let mut wanted: Vec<String> = listed.iter().map(|s| (*s).to_owned()).collect();
        wanted.sort();
        assert_eq!(actual, wanted, "cases that differ from the OCaml printers (actual files in the temp dir as flow_web_actual_*)");
    }
}

#[test]
fn fast_cases_match_the_ocaml_printers() {
    let mut diffs = Diffs(Vec::new());
    for (name, source) in fast_cases() {
        let name = format!("f_{name}");
        let program = compile(source);
        diffs.same("wgsl_fast", &name, &wgsl_text(wgsl::kernel_fast(&program)), &expected("wgsl_fast", &name, ".wgsl"));
        diffs.region(&name, &program);
    }
    diffs.finish(&[]);
}

#[test]
fn carried_fixtures_match_the_ocaml_printers() {
    let mut diffs = Diffs(Vec::new());
    for (name, program) in carried_fixtures(4096) {
        let name = format!("c4096_{name}");
        diffs.same("wgsl_fast", &name, &wgsl_text(wgsl::kernel_fast(&program)), &expected("wgsl_fast", &name, ".wgsl"));
        diffs.region(&name, &program);
    }
    diffs.finish(&[]);
}

#[test]
fn error_cases_match_the_ocaml_printers() {
    let mut diffs = Diffs(Vec::new());
    for (name, source) in error_cases() {
        let program = compile(source);
        diffs.same("wgsl_error", name, &wgsl_text(wgsl::kernel_fast(&program)), &expected("wgsl_error", name, ".wgsl"));
    }
    diffs.finish(&[]);
}

#[test]
fn test_js_programs_match_the_ocaml_printers() {
    let mut diffs = Diffs(Vec::new());
    for (name, program) in crate::test_js::programs().into_iter().chain(crate::test_js::carried()) {
        diffs.region(&name, &program);
        diffs.same("wgsl", &name, &wgsl_text(wgsl::kernel(false, &program)), &expected("wgsl", &name, ".wgsl"));
    }
    diffs.finish(&[]);
}
