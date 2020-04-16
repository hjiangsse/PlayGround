#[derive(Debug)]
enum IpAddrType {
    V4(String),
    V6(String),
}

#[derive(Debug)]
enum IpAddrNewType {
    V4(u32, u32, u32, u32),
    V6(String),
}

//struct can be enum variables
#[derive(Debug)]
struct BlackTea {
    name: String,
    price: u32,
    age: u32,
}

#[derive(Debug)]
struct GreenTea {
    name: String,
    price: u32,
}

#[derive(Debug)]
enum TeaType {
    BLACK(BlackTea),
    GREEN(GreenTea),
}

#[derive(Debug)]
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}

impl Message {
    fn call(&self) {
        println!("write message to remote addr: {:?}", self);
    }
}

fn main() {
    let four = IpAddrType::V4(String::from("127.0.0.1"));
    let six = IpAddrType::V6(String::from("::1"));

    println!("value of four: {:?}", four);
    println!("value of six: {:?}", six);

    let new_four = IpAddrNewType::V4(127, 0, 0, 1);
    let new_six = IpAddrNewType::V6(String::from("::1"));

    println!("value of four: {:?}", new_four);
    println!("value of six: {:?}", new_six);

    let bl_tea = TeaType::BLACK(BlackTea {
        name: String::from("yunnan puer"),
        price: 500,
        age: 3,
    });

    let gr_tea = TeaType::GREEN(GreenTea {
        name: String::from("biluochun"),
        price: 700,
    });

    println!("bl_tea: {:?}", bl_tea);
    println!("gr_tea: {:?}", gr_tea);

    let msg = Message::Write(String::from("hjiang"));
    msg.call();

    let some_number = Some(5);
    let some_string = Some("This is a string");

    let absent_number: option<u32> = None;
}
