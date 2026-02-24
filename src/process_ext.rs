//! Cross-platform process exec helper.
//!
//! On Unix, uses `CommandExt::exec()` to replace the current process.
//! On Windows, spawns the command and exits with its status code.

use std::process::Command;

/// Replace the current process with the given command (Unix) or
/// spawn it and exit with its status code (Windows).
///
/// This function never returns on success.
#[cfg(unix)]
pub fn exec_command(cmd: &mut Command) -> ! {
    use std::os::unix::process::CommandExt;
    let err = cmd.exec();
    eprintln!("exec failed: {}", err);
    std::process::exit(1);
}

#[cfg(windows)]
pub fn exec_command(cmd: &mut Command) -> ! {
    match cmd.status() {
        Ok(s) => std::process::exit(s.code().unwrap_or(1)),
        Err(e) => {
            eprintln!("spawn failed: {}", e);
            std::process::exit(1);
        }
    }
}

/// Collect environment variables that the daemon child needs.
///
/// The daemon inherits the daemon's env (not Cargo's), so we transmit
/// relevant env vars (CARGO_PKG_*, build-script vars, etc.) for the
/// child to set before compilation.
pub fn collect_daemon_env_vars() -> Vec<(String, String)> {
    std::env::vars()
        .filter(|(key, _)| {
            key.starts_with("CARGO_PKG_")
                || key.starts_with("CARGO_MANIFEST_")
                || key.starts_with("CARGO_SLICER_")
                || key.starts_with("DEP_")
                || key == "OUT_DIR"
                || key == "TARGET"
                || key == "HOST"
                || key == "OPT_LEVEL"
                || key == "PROFILE"
                || key == "CARGO_CFG_TARGET_OS"
                || key == "CARGO_CFG_TARGET_ARCH"
                // Custom build-script env vars (uppercase + underscore names)
                || (key.chars().all(|c| c.is_ascii_uppercase() || c == '_' || c.is_ascii_digit())
                    && !key.starts_with("RUSTC")
                    && !key.starts_with("RUST")
                    && key != "PATH"
                    && key != "HOME"
                    && key != "USER"
                    && key != "SHELL"
                    && key != "TERM"
                    && key != "LANG"
                    && key != "PWD"
                    && key != "SHLVL"
                    && key != "OLDPWD"
                    && key != "HOSTNAME"
                    && key != "LOGNAME"
                    && key != "MAIL"
                    && key != "EDITOR"
                    && key != "VISUAL"
                    && key != "XDG_RUNTIME_DIR"
                    && key != "_"
                    && key != "USERPROFILE"
                    && key != "APPDATA"
                    && key != "LOCALAPPDATA"
                    && key != "SYSTEMROOT"
                    && key != "WINDIR"
                    && key != "COMSPEC"
                    && key != "TEMP"
                    && key != "TMP")
        })
        .collect()
}
