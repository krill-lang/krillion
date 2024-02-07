use std::panic::*;
use std::thread;
use backtrace::*;

const MAX_BACKTRACE: usize = 10;

pub fn setup() {
    set_hook(Box::new(|info| {
        println!("\x1b[1;31mCompiler panic: details below\x1b[0m");
        if let Some(thread) = thread::current().name() {
            println!("  \x1b[1mThread:\x1b[0m `{thread}`");
        }
        if let Some(location) = info.location() {
            println!("  \x1b[1mLocation:\x1b[0m `{location}`");
        }
        if let Some(payload) = info.payload().downcast_ref::<&'static str>() {
            println!("  \x1b[1mMessage:\x1b[0m `{payload}`");
        }

        println!("  \x1b[1mStack trace:\x1b[0m");
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

                    if !skipping {
                        if !hit {
                            print!("      \x1b[1m{nth}:\x1b[0m ");
                        } else {
                            print!("         ");
                        }

                        if let Some(name) = symbol.name() {
                            println!("{name:#}");
                        } else {
                            println!("<unnamed>");
                        }

                        if let (
                            Some(f),
                            Some(l),
                            Some(c)
                        ) = (
                            symbol.filename(),
                            symbol.lineno(),
                            symbol.colno()
                        ) {
                            println!("            \x1b[1mAt\x1b[0m {}:{l}:{c}", f.display());
                        }

                        hit = true;
                    }
                });

                nth += hit as usize;

                nth < MAX_BACKTRACE
            })
        }

        println!("\n\
\x1b[1;32mInfo: please report this to \x1b[0;4;34mhttps://github.com/krill-lang/krillion/issues\
\x1b[0;1;32m if this crash is not intentional\x1b[0m");
    }));
}
