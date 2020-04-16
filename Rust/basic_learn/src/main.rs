fn main() {
    /*
    let mut x = 5;
    println!("The value of x is: {}", x);
    x = 6;
    println!("The value of x is: {}", x);

    const MAX_POINT: u32 = 100_000;
    println!("The value of the const MAX_POINT is: {}", MAX_POINT);

    func1();
    func2();

    let x = 5;
    let x = x + 1;
    let x = x * 2;
    println!("The value of x is: {}", x);


    let tup: (i32, f64, u8) = (500, 3.1, 1);
    let (x, y, z) = tup;
    println!("The value x: {}, y: {}, z: {}", x, y, z);
    println!("The value of tup.x: {}", tup.0);
    println!("The value of tup.y: {}", tup.1);
    println!("The value of tup.z: {}", tup.2);

    let x = [3; 5];
    println!("The value of x[0]: {}", x[0]);
    println!("The value of x[4]: {}", x[4]);
     */
    //another_function(10, 2.0);
    println!("{}", identity(10));
    println!("{}", square(identity(20)));
}

fn identity(x: u32) -> u32 {
    x
}

fn square(x: u32) -> u32 {
    x * x
}

/*
fn another_function(x: i32, y: f32) {
    println!("The value of x is: {}", x);
    println!("The value of y is: {}", y);
}
*/

/*
fn func1() {
    const CONS1_VAR: u32 = 100;
    println!("The value of the const CONS1_VAR: {}", CONS1_VAR);
}

fn func2() {
    const CONS2_VAR: u32 = 200;
    println!("The value of the const CONS1_VAR: {}", CONS2_VAR);
}
*/
