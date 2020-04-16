fn main() {
    let s = String::new();
    println!("the value of s is: {}", s);

    let data = "initial contents";
    let ns = data.to_string();
    println!("the value of ns is: {}", ns);

    let mut ms = String::from("This ");
    ms.push_str("is a string");
    println!("The value of ms: {}", ms);

    let aps = "haha";
    ms.push_str(aps);
    println!("The value of ms: {}", ms);
    println!("The value of aps: {}", aps);

    ms.push('.');
    println!("The value of ms: {}", ms);

    let s1 = String::from("first");
    let s2 = String::from("second");
    let s3 = s1 + &s2;
    println!("{}", s3);
}
