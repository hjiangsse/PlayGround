fn main() {
    /*
    let r;

    let x = 5;
    r = &x;
    println!("r: {}", r);
     */
    //let end = first_word_1(&String::from("This is the end"));
    //println!("The end point of the first word: {}", end);
    /*
    let sentense = String::from("This is the end");
    let firstword = first_word_2(&sentense);
    println!("The end point of the first word: {}", firstword);

    let first = String::from("abcd");
    let second = "xyz";
    let longer = longest(&first, &second);
    println!("The longer of the two: {}", longer);
     */

    let string1 = String::from("long string is long");
    let result: &str;
    {
        let string2 = String::from("xyz");
        let result = longest(&string1, &string2);
    }
    println!("The longest string is {}", result);
}

fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}

/*
fn first_word_1(s: &String) -> usize {
    let bytes = s.as_bytes();
    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return i;
        }
    }

    s.len()
}

fn first_word_2(s: &String) -> &str {
    let bytes = s.as_bytes();
    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[..i];
        }
    }

    &s[..]
}
*/
