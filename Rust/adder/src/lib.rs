#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn larger_can_hold_smaller() {
        let larger = Rectangle {
            width: 8,
            height: 7,
        };

        let smaller = Rectangle {
            width: 5,
            height: 3,
        };

        assert!(larger.can_hold(&smaller))
    }

    #[test]
    fn smaller_can_hold_larger() {
        let larger = Rectangle {
            width: 8,
            height: 7,
        };

        let smaller = Rectangle {
            width: 5,
            height: 3,
        };

        assert!(!smaller.can_hold(&larger))
    }

    #[test]
    fn it_add_two() {
        assert_eq!(add_two(10), 12);
        assert_eq!(add_two(11), 13);
    }

    #[test]
    #[should_panic]
    fn greater_than_100() {
        Guess::new(200);
    }

    #[test]
    fn this_test_will_pass() {
        let value = prints_and_returns_10(4);
        assert_eq!(10, value)
    }

    #[test]
    fn this_test_will_fail() {
        let value = prints_and_returns_10(8);
        assert_eq!(10, value)
    }

    #[test]
    fn add_two_and_two() {
        assert_eq!(4, add_two(2));
    }

    #[test]
    fn add_two_and_three() {
        assert_eq!(5, add_two(3));
    }

    #[test]
    #[ignore]
    fn add_two_and_hundred() {
        assert_eq!(102, add_two(100));
    }

    #[test]
    fn internal_test() {
        assert_eq!(10, internal_adder(4, 6));
    }
}

fn internal_adder(a: i32, b: i32) -> i32 {
    a + b
}

#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn can_hold(&self, other: &Rectangle) -> bool {
        self.width > other.width && self.height > other.height
    }
}

pub fn add_two(a: i32) -> i32 {
    a + 2
}

#[derive(Debug)]
struct Guess {
    value: i32,
}

impl Guess {
    pub fn new(value: i32) -> Guess {
        if value < 1 || value > 100 {
            panic!("guess value must between 1 and 100, got {}.", value);
        }

        Guess { value }
    }
}

fn prints_and_returns_10(a: i32) -> i32 {
    println!("I got the value {}", a);
    return 10;
}
