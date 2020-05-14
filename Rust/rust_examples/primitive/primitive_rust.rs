use std::fmt;
use std::mem;

fn main() {
    let logical = true;
    let no_logical: bool = false;
    println!("{} logical, {} no logical", logical, no_logical);
    let logical = 100;
    let no_logical = 1000;
    println!("{} logical, {} no logical", logical, no_logical);

    println!("1 + 2 = {}", 1u32 + 2);
    println!("1 - 2 = {}", 1i32 - 2);
    //println!("1 - 2 = {}", 1u32 - 2);

    //tuples
    let stu = ("hjiang", 29);
    let revstu = reverse(stu);
    println!("the reverse of {:?} is {:?}", stu, revstu);

    println!("one element tuple: {:?}", (5u32,));
    println!("just element: {}", (5u32));

    //test matrix
    let mut matrix = Matrix(1.1, 2.2, 3.3, 4.4);
    println!("debug output: {:?}", matrix);
    println!("display output: {}", matrix);

    transpose(&mut matrix);
    println!("after transpose: ");
    println!("debug output: {:?}", matrix);
    println!("display output: {}", matrix);

    //array and slice
    let xs = [1, 2, 3, 4];
    println!("the array xs is {:?}", xs);
    println!("the size of the array is: {}", xs.len());
    for (idx, elm) in xs.iter().enumerate() {
        println!("index: {}, value: {}", idx, elm);
    }

    println!("xs occupies {} bytes", mem::size_of_val(&xs));
    analyze_slice(&xs);
}

fn reverse<T1, T2>(pair: (T1, T2)) -> (T2, T1) {
    let (first, second) = pair;
    (second, first)
}

#[derive(Debug)]
struct Matrix(f32, f32, f32, f32);

impl fmt::Display for Matrix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\n({} {})", self.0, self.1)?;
        write!(f, "\n({} {})", self.2, self.3)
    }
}

fn transpose(m: &mut Matrix) {
    let tmp = m.1;
    m.1 = m.2;
    m.2 = tmp;
}

fn analyze_slice(slice: &[u32]) {
    println!("first element in the slice: {}", slice[0]);
    println!("second element in the slice: {}", slice[1]);
    println!("the slice has {} elements", slice.len())
}
