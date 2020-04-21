use std::fmt;

#[derive(Debug)]
struct Person<'a> {
    name: &'a str,
    age: u8,
}

impl fmt::Display for Person<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[name: {}, age: {}]", self.name, self.age)
    }
}

fn new_person(name: &str, age: u8) -> Person {
    Person {
        name: name,
        age: age,
    }
}

struct Point {
    x: f32,
    y: f32,
}

#[allow(dead_code)]
struct Rectangle {
    top_left: Point,
    bottom_right: Point,
}

struct Pair(i32, f32);

struct Nil;

fn rect_area(rect: &Rectangle) -> f32 {
    let Rectangle {
        top_left: t,
        bottom_right: b,
    } = rect;

    let width = b.x - t.x;
    let height = b.y - t.y;

    return width.abs() * height.abs();
}

fn main() {
    let name = String::from("marry");
    let age = 88;
    let person = new_person(&name, age);
    println!("{}", person);

    let point: Point = Point { x: 10.3, y: 10.4 };
    println!("coordinates of point: {} {}", point.x, point.y);

    let bottom_right: Point = Point { x: 15.2, y: 5.4 };
    println!(
        "coordinates of bottom_right: {} {}",
        bottom_right.x, bottom_right.y
    );

    //destructing the Point struct
    let Point {
        x: top_edge,
        y: left_edge,
    } = bottom_right;

    println!("top_edge of the bottom_right: {}", top_edge);
    println!("left_edge of the bottom_right: {}", left_edge);

    let pair = Pair(1, 3.2);
    println!("the first element of the pair: {}", pair.0);
    println!("the second element of the pair: {}", pair.1);

    let rectangle = Rectangle {
        top_left: point,
        bottom_right: bottom_right,
    };
    println!("the area of this rectangle is: {}", rect_area(&rectangle));
}
