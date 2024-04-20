use std::process::Command;

fn main() {
    let branch = Command::new("git")
        .args(["rev-parse", "--abbrev-ref", "HEAD"])
        .output()
        .ok()
        .and_then(|out| String::from_utf8(out.stdout).ok())
        .unwrap_or("".to_string());

    let mut commit = Command::new("git")
        .args(["rev-parse", "HEAD"])
        .output()
        .ok()
        .and_then(|out| String::from_utf8(out.stdout).ok())
        .map(|mut c| {
            c.truncate(7);
            c
        })
        .unwrap_or(r"¯\_(ツ)_/¯".to_string());

    if matches!(
        Command::new("git")
            .args(["diff-index", "--quiet", "HEAD"])
            .status()
            .ok()
            .map(|a| !a.success()),
        Some(true)
    ) {
        commit += " (modified)"
    }

    println!("cargo:rustc-env=COMMIT={commit}");

    println!("cargo:rerun-if-changed=.git/refs/heads/{branch}");
    println!("cargo:rerun-if-changed=.git/HEAD");
    println!("cargo:rerun-if-changed=src");
}
