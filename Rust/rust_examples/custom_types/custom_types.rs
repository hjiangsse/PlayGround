#[derive(Debug)]
struct Point {
    x: f32,
    y: f32,
}

#[derive(Debug)]
struct Rectangle {
    top_left: Point,
    bottom_right: Point,
}

fn rect_area(rec: &Rectangle) -> f32 {
    let width = rec.bottom_right.x - rec.top_left.x;
    let height = rec.top_left.y - rec.bottom_right.y;
    let area = width * height;
    if area < 0.0 {
        return -area;
    }
    area
}

fn create_square(pt: Point, factor: f32) -> Rectangle {
    Rectangle {
        top_left: Point {
            x: pt.x,
            y: pt.y + factor,
        },
        bottom_right: Point {
            x: pt.x + factor,
            y: pt.y,
        },
    }
}

enum WebEvent {
    PageLoad,
    PageUnload,
    KeyPress(char),
    Paste(String),
    Click { x: f32, y: f32 },
}

fn inspect_event(evt: WebEvent) {
    match evt {
        WebEvent::PageLoad => println!("page loaded"),
        WebEvent::PageUnload => println!("page unloaded"),
        WebEvent::KeyPress(c) => println!("the user just press '{}' key", c),
        WebEvent::Paste(str) => println!("the user just paste a string {}", str),
        WebEvent::Click { x, y } => println!("the user just click in point {} {}", x, y),
    }
}

#[derive(Debug)]
enum VeryVerboseEnumOfThingsToDoWithNumbers {
    Add,
    Substract,
}

//a type alias
type Operations = VeryVerboseEnumOfThingsToDoWithNumbers;

impl VeryVerboseEnumOfThingsToDoWithNumbers {
    fn run(&self, x: i32, y: i32) -> i32 {
        match self {
            Self::Add => x + y,
            Self::Substract => x - y,
        }
    }
}

#[allow(dead_code)]
enum Status {
    Rich,
    Poor,
}

enum Work {
    Civilian,
    Soldier,
}

enum Number {
    Zero,
    One,
    Two,
}

enum Color {
    Red = 0xff0000,
    Green = 0x00ff00,
    Blue = 0x0000ff,
}

use crate::Status::*;
use crate::Work::*;

fn main() {
    println!("Zero is: {}", Number::Zero as i32);
    println!("One is: {}", Number::One as i32);
    println!("Two is: {}", Number::Two as i32);

    println!("Roses are #{:06x}", Color::Red as i32);
    println!("Violets are #{:06x}", Color::Blue as i32);
    println!("Grasses are #{:06x}", Color::Green as i32);

    let rect: Rectangle = Rectangle {
        top_left: Point { x: 1.0, y: 8.0 },
        bottom_right: Point { x: 8.0, y: 4.0 },
    };
    println!("the debug value of this rect: {:?}", rect);
    println!("the area value of this rect: {}", rect_area(&rect));

    let squre: Rectangle = create_square(Point { x: 0.0, y: 0.0 }, 10.0);
    println!("the debug value of this squre: {:?}", squre);
    println!("the area of this squre: {}", rect_area(&squre));

    let web_event = WebEvent::PageLoad;
    inspect_event(web_event);
    let web_event = WebEvent::PageUnload;
    inspect_event(web_event);
    let web_event = WebEvent::KeyPress('d');
    inspect_event(web_event);
    let web_event = WebEvent::Paste(String::from("This is a test line"));
    inspect_event(web_event);
    let web_event = WebEvent::Click { x: 1.0, y: 2.0 };
    inspect_event(web_event);

    let x = Operations::Add;
    println!("{:?}", x);
    let y = Operations::Substract;
    println!("{:?}", y);

    let res1 = x.run(10, 5);
    println!("res: {}", res1);
    let res2 = y.run(10, 5);
    println!("res: {}", res2);

    let status = Poor;
    let work = Civilian;

    match status {
        Poor => println!("This is a poor life, but maybe not a bad life!"),
        Rich => println!("The real reach people has good spritual status!"),
    }

    match work {
        Civilian => println!("I am a molecule of this socity!"),
        Soldier => println!("The gun is my life!"),
    }
}
