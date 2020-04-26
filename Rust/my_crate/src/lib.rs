//!#My Crate
//!
//!`my_crate` is a collection of utilities to make performing contain
//!calculations more convenient    

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

///Adds one to the number given
///
///# Examples
///```
///    let arg = 5;
///    let answer = my_crate::add_one(arg);
///
///    assert_eq!(6, answer);
///```
///# Panics
///# Errors
///# Safety
pub fn add_one(x: i32) -> i32 {
    x + 1
}
