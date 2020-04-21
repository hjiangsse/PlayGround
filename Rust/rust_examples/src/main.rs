use std::fmt;
use std::mem;

struct Matrix {
    a: i32,
    b: i32,
    c: i32,
    d: i32,
}

impl fmt::Display for Matrix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\n({},{})\n({},{})", self.a, self.b, self.c, self.d)
    }
}

impl Matrix {
    fn transpose(&self) -> Matrix {
        let res = Matrix {
            a: self.a,
            b: self.c,
            c: self.b,
            d: self.d,
        };
        res
    }
}

fn main() {
    test_scalar_type();
    test_tuple();
    test_arr_slice();
}

fn test_arr_slice() {
    //let test = String::from("hello");
    let x: [i32; 100] = [10; 100];
    println!("the size of the array: {}", mem::size_of_val(&x));
}

fn reverse_tuple(old: (i32, bool)) -> (bool, i32) {
    let (a, b) = old;
    (b, a)
}

fn general_reverse_tuple<T, U>(old: (T, U)) -> (U, T) {
    let (a, b) = old;
    (b, a)
}

fn test_tuple() {
    let long_tuple = (
        1u8, 2u16, 3u32, 4u64, -1i8, -2i16, -3i32, -4i64, 0.1f32, 0.2f64, 'a', true,
    );
    println!("the long tuple is: {:?}", long_tuple);

    println!("the first element of the long_tuple is: {}", long_tuple.0);
    println!("the second element of the long_tuple is: {}", long_tuple.1);

    //tuple inside tuple
    let out_tuple = ((1, 2, 3), 4, (true, false));
    println!("the long tuple is: {:?}", out_tuple);

    let old = (10, false);
    println!("The reverse of the old: {:?}", reverse_tuple(old));

    let gen_old = (String::from("left"), String::from("right"));
    println!(
        "The reverse of the gen_old: {:?}",
        general_reverse_tuple(gen_old)
    );
    /*
    let too_long_tuple = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    println!("the too long tuple is: {:?}", too_long_tuple);
     */

    let matrix = Matrix {
        a: 1,
        b: 2,
        c: 3,
        d: 4,
    };
    println!("matrix is: {}", matrix);
    println!("transpose is: {}", matrix.transpose());
}

fn test_scalar_type() {
    let x: i128 = 10;
    println!("{}", mem::size_of_val(&x));

    let y: isize = 10;
    println!("{}", mem::size_of_val(&y));

    let z: char = 'a';
    println!("{}", mem::size_of_val(&z));

    let w: &str = "Thisme横";
    println!("{}", mem::size_of_val(w));

    let u: () = ();
    println!("{}", mem::size_of_val(&u));

    let v = 0.000_001;
    println!("{} {}", v, mem::size_of_val(&v));
}
