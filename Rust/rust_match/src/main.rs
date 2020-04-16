/*
#[derive(Debug)]
enum UsState {
    Alabama,
    Alaska,
}

enum Coin {
    Penny,
    Nickel,
    Dime,
    Quarter(UsState),
}

fn plus_one(x: Option<i32>) -> Option<i32> {
    match x {
        None => None,
        Some(i) => Some(i + 1),
    }
}

fn value_in_cents(coin: Coin) -> u8 {
    match coin {
        Coin::Penny => 1,
        Coin::Nickel => {
            println!("Lucky Nickel");
            5
        }
        Coin::Dime => 10,
        Coin::Quarter(state) => {
            println!("State quarter from {:?}!", state);
            25
        }
    }
}
*/

fn main() {
    let some_u8_value = Some(0u8);
    if let Some(0) = some_u8_value {
        println!("three");
    }
    /*
    let unknow_coin = Coin::Nickel;
    println!("value of the unknow coin: {}", value_in_cents(unknow_coin));

    let alabama_coin = Coin::Quarter(UsState::Alabama);
    println!("value of alabama_coin: {}", value_in_cents(alabama_coin));

    let five = Some(5);
    let six = plus_one(five);
    let none = plus_one(None);
    println!("The value of five: {:?}", five);
    println!("The value of six: {:?}", six);
    println!("The value of none: {:?}", none);

    let some_u8_val = 10u8;
    match some_u8_val {
        1 => println!("One"),
        3 => println!("Three"),
        5 => println!("five"),
        _ => println!("Oh"),
    }
    */
}
