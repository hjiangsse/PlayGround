use std::env;
use std::process;

use minigrep::Config;

fn main() {
    /*
    let args: Vec<String> = env::args().collect();

    for arg in env::args() {
        println!("{}", arg);
    }
    */

    let config = Config::iternew(env::args()).unwrap_or_else(|err| {
        eprintln!("Problem parsing arguments: {}", err);
        process::exit(1)
    });

    /*
    let config = Config::new(&args).unwrap_or_else(|err| {
        eprintln!("Problem parsing arguments: {}", err);
        process::exit(1)
    });
    */

    minigrep::run(config).unwrap_or_else(|err| {
        eprintln!("application error: {}", err);
        process::exit(1)
    })
}
