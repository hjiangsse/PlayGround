//use std::any::type_name;

fn main() {
    /*
    let mut s = String::from("This is the end");
    let word = first_word(&s);
    println!("The first word is {}", word);
    s.clear();

    let mut s = String::from("hello world");
    let hello = &s[0..5];
    let word = &s[5..];
    println!("the first word: {}", hello);
    println!("the second word: {}", word);

    let word = first_word(&s);
    s.clear();
    println!("The first word is: {}", word);
    */
    //println!("first_word: {}", first_word(&s));
    //let s = "Hello Slice";
    //println!("{}", type_of(s));
    //println!("new first word: {}", new_first(s));
    let x = 10;
    let y = x;
    println!("now x is {}", x);
    println!("now y is {}", y);

    let str1 = String::from("Hello World");
    let str2 = str1;
    //println!("Now str1 is: {}", str1);
    println!("Now str2 is: {}", str2);
}

/*
fn new_first(s: &str) -> &str {
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[0..i];
        }
    }

    &s[..]
}
*/

/*
fn first_word(s: &String) -> &str {
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[0..i];
        }
    }

    &s[..]
}
*/

/*
fn first_word(s: &String) -> usize {
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return i;
        }
    }

    s.len()
}
*/
