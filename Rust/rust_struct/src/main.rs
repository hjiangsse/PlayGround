/*
struct User {
    username: String,
    email: String,
    sign_in_count: u64,
    active: bool,
}

fn build_user(name: &str, email: &str) -> User {
    User {
        username: String::from(name),
        email: String::from(email),
        active: true,
        sign_in_count: 1,
    }
}

fn build_user_shorthand(username: String, email: String) -> User {
    User {
        username,
        email,
        active: true,
        sign_in_count: 1,
    }
}

fn print_user(user: &User) {
    println!("--------the content of this user--------");
    println!("username: {}", user.username);
    println!("email: {}", user.email);
    println!("sign_in_count: {}", user.sign_in_count);
    println!("active: {}", user.active);
}

//struct updating syntax
//tuple struct
struct Color(i32, i32, i32);
struct Point(i32, i32, i32);
*/

#[derive(Debug)]

struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    /*
    fn area(&self) -> u32 {
        self.width * self.height
    }

    fn set_width(&mut self, wid: u32) {
        self.width = wid;
    }

    fn set_height(&mut self, hei: u32) {
        self.height = hei;
    }
    */

    fn can_hold(&self, other: &Rectangle) -> bool {
        self.width > other.width && self.height > other.height
    }

    fn square(size: u32) -> Rectangle {
        Rectangle {
            width: size,
            height: size,
        }
    }
}

fn main() {
    let rect1 = Rectangle {
        width: 30,
        height: 50,
    };

    let rect2 = Rectangle {
        width: 10,
        height: 40,
    };

    let rect3 = Rectangle {
        width: 60,
        height: 45,
    };

    println!("Can rect1 hold rect2? {}", rect1.can_hold(&rect2));
    println!("Can rect1 hold rect3? {}", rect1.can_hold(&rect3));

    let square_rect = Rectangle::square(20);
    println!("square rectangle: {:#?}", square_rect)
    /*
    let mut rect1 = Rectangle {
        width: 10,
        height: 20,
    };
    println!("The area of the rect1: {}", area(&rect1));
    println!("Use methods get the area: {}", rect1.area());
    println!("Rect1 is {:?}", rect1);
    println!("Rect1 is {:#?}", rect1);

    rect1.set_width(20);
    println!("Use methods get the area: {}", rect1.area());

    let user1 = User {
        email: String::from("hjiang@sse.com.cn"),
        username: String::from("hjiang"),
        active: true,
        sign_in_count: 1,
    };
    print_user(&user1);

    let user2 = build_user("jiang", "jiang@163.com");
    print_user(&user2);

    let user3 = build_user_shorthand(String::from("heng"), String::from("heng@126.com"));
    print_user(&user3);

    let user4 = User {
        email: String::from("xixi@163.com"),
        //username: String::from("xixi"),
        ..user3
    };
    print_user(&user4);

    //test tuple struct
    let black = Color(0, 0, 0);
    println!("The black_r: {}", black.0);
    println!("The black_g: {}", black.1);
    println!("The black_b: {}", black.2);


    let (black_r, black_g, black_b) = black;
    println!("black_r: {}", black_r);
    println!("black_g: {}", black_g);
    println!("black_b: {}", black_b);
    */
}
