use add_one;
use rand::prelude::*;

fn main() {
    let num = 10;
    println!("{} plus one is {}", num, add_one::add_one(num));

    let mut rng = rand::thread_rng();
    let num: u64 = rng.next_u64();
    println!("rand gen number: {}", num);

    println!("{} add a random i32 is {}", 10, add_one::add_rnd(10));
}
