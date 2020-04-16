fn main() {
    let user1 = Some(String::from("hjiang"));
    greet_user(user1);
    let user2 = None;
    greet_user(user2);

    let squares: Vec<_> = (0..10).map(|i| i * i).collect();
    for &elem in squares.iter() {
        println!("{}", elem);
    }
}

fn greet_user(name: Option<String>) {
    match name {
        Some(name) => println!("hello, {}", name),
        None => println!("sorry, we do not know your name"),
    }
}
