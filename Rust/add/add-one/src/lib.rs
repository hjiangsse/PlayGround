use rand::prelude::*;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

///add one to a i32 number
pub fn add_one(x: i32) -> i32 {
    x + 1
}

///add a random number to x
pub fn add_rnd(x: i32) -> i32 {
    let mut rng = rand::thread_rng();
    let adder: i32 = rng.gen::<i32>();
    x + adder
}
