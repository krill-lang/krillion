use std::panic::*;
use std::thread;
use backtrace::*;
use panic_message::*;

const MAX_BACKTRACE: usize = 25;

pub fn init() {
    set_hook(Box::new(|info| {
        eprintln!("\x1b[1;31mCompiler panic: details below\x1b[0m");
        eprintln!("  \x1b[1mCompiler commit:\x1b[0m `{}`", env!("COMMIT"));
        if let Some(thread) = thread::current().name() {
            eprintln!("  \x1b[1mThread:\x1b[0m `{thread}`");
        }
        if let Some(location) = info.location() {
            eprintln!("  \x1b[1mLocation:\x1b[0m `{location}`");
        }
        if let Some(message) = get_panic_info_message(info) {
            eprintln!("  \x1b[1mMessage:\x1b[0m `{message}`");
        }

        eprintln!("  \x1b[1mStack backtrace:\x1b[0m");
        eprintln!("    \x1b[2m...\x1b[0m");
        unsafe {
            let mut skipping = true;
            let mut nth = 0;
            trace_unsynchronized(|frame| {
                let mut hit = false;
                resolve_unsynchronized(frame.ip(), |symbol| {
                    let name = symbol.name().and_then(|a| a.as_str());
                    if let Some(name) = name {
                        if name.contains("__rust_begin_short_backtrace") {
                            skipping = true;
                            return;
                        } else if name.contains("__rust_end_short_backtrace")
                            || name.contains("rust_begin_unwind") {
                            skipping = false;
                            return;
                        }
                    }

                    if !skipping && nth < MAX_BACKTRACE {
                        eprint!("\x1b[1A");
                        if !hit {
                            eprint!("    \x1b[1m{nth}:\x1b[0m ");
                        } else {
                            eprint!("       ");
                        }

                        if let Some(name) = symbol.name() {
                            eprintln!("{name:#}");
                        } else {
                            eprintln!("<unnamed>");
                        }

                        if let (Some(f), Some(l)) = (symbol.filename(), symbol.lineno()) {
                            eprint!("          \x1b[1mAt\x1b[0m {}:{l}", f.display());
                            if let Some(c) = symbol.colno() { eprint!(":{c}"); }
                            eprintln!();
                        }

                        eprintln!("    \x1b[2m...\x1b[0m");
                        hit = true;
                    }
                });

                nth += hit as usize;

                true
            })
        }

        eprintln!("\x1b[1A\x1b[2K\n\
\x1b[1;32mInfo: please report this to \x1b[0;4;34mhttps://github.com/krill-lang/krillion/issues\
\x1b[0;1;32m if this crash is not intentional\x1b[0m\n");
    }));
}
