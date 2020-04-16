use std::fs::File;
use std::io;
use std::io::Read;
//use std::io::ErrorKind;

fn read_username_from_file() -> Result<String, io::Error> {
    let f = File::open("test.txt");

    let mut f = match f {
        Ok(file) => file,
        Err(e) => return Err(e),
    };

    let mut s = String::new();

    match f.read_to_string(&mut s) {
        Ok(_) => Ok(s),
        Err(e) => Err(e),
    }
}

fn read_username_from_file_new() -> Result<String, io::Error> {
    let mut f = File::open("test.txt")?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    Ok(s)
}

fn main() {
    let res = read_username_from_file();
    match res {
        Ok(s) => println!("username of file: {:}", s),
        Err(e) => panic!("get username error: {:?}", e),
    }

    let res2 = read_username_from_file_new();
    match res2 {
        Ok(s) => println!("username of file: {:}", s),
        Err(e) => panic!("get username error: {:?}", e),
    }
    /*
    let f = File::open("test.txt");

    let _f = match f {
        Ok(file) => file,
        Err(error) => match error.kind() {
            ErrorKind::NotFound => match File::create("test.txt") {
                Ok(fc) => fc,
                Err(e) => panic!("Problem creating the file: {:?}", e),
            },
            other_error => panic!("Problem opening the file: {:?}", other_error),
        },
    };
     */
    //let f = File::open("test.txt").unwrap();
    //let f = File::open("test.txt").expect("Failed to open test.txt");
}
