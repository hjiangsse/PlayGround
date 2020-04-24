use std::env;
use std::error::Error;
use std::fs;

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(config.filename)?;

    let results = if config.case_sensitive {
        search(&config.query, &contents)
    } else {
        intensive_search(&config.query, &contents)
    };

    for line in results {
        println!("{}", line);
    }

    Ok(())
}

#[derive(Debug)]
pub struct Config {
    pub query: String,
    pub filename: String,
    pub case_sensitive: bool,
}

impl Config {
    pub fn iternew(mut args: std::env::Args) -> Result<Config, &'static str> {
        args.next();

        let query = match args.next() {
            Some(arg) => arg,
            None => return Err("do not get a query string"),
        };

        let filename = match args.next() {
            Some(arg) => arg,
            None => return Err("do not get a filename string"),
        };

        let case_sensitive = env::var("CASE_INSENSITIVE").is_err();

        Ok(Config {
            query,
            filename,
            case_sensitive,
        })
    }

    pub fn new(args: &[String]) -> Result<Config, &'static str> {
        if args.len() < 3 {
            return Err("not enough arguments");
        }

        let query = args[1].clone();
        let filename = args[2].clone();

        let case_sensitive = env::var("CASE_INSENSITIVE").is_err();

        Ok(Config {
            query,
            filename,
            case_sensitive,
        })
    }
}

impl PartialEq for Config {
    fn eq(&self, other: &Self) -> bool {
        self.query == other.query
            && self.filename == other.filename
            && self.case_sensitive == other.case_sensitive
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_new() {
        let args = vec![String::from("arg1"), String::from("arg2")];
        let res = Config::new(&args);

        let wanted = Config {
            query: String::from("arg1"),
            filename: String::from("arg2"),
            case_sensitive: false,
        };

        match res {
            Ok(v) => assert_eq!(wanted, v),
            Err(_) => {}
        }
    }

    #[test]
    fn one_result() {
        let query = "duct";
        let contents = "\
Rust:
safe, fast, productive.
Pick three.";
        assert_eq!(vec!["safe, fast, productive."], search(query, contents));
    }

    #[test]
    fn case_intensitive() {
        let query = "rUsT";
        let contents = "\
Rust:
safe, fast, productive.
Trust it!";
        assert_eq!(
            vec!["Rust:", "Trust it!"],
            intensive_search(query, contents)
        );
    }

    #[test]
    fn test_itersearch() {
        let query = "duct";
        let contents = "\
Rust:
safe, fast, productive.
Pick three.";
        assert_eq!(vec!["safe, fast, productive."], itersearch(query, contents));
    }
}

pub fn itersearch<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    contents
        .lines()
        .filter(|line| line.contains(query))
        .collect()
}

pub fn search<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    let mut res = Vec::new();

    for line in contents.lines() {
        if line.contains(query) {
            res.push(line);
        }
    }

    res
}

pub fn intensive_search<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    let mut res = Vec::new();
    let low_query = query.to_lowercase();

    for line in contents.lines() {
        let low_line = line.to_lowercase();
        if low_line.contains(&low_query) {
            res.push(line);
        }
    }

    res
}
