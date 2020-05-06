enum List {
    Cons(i32, Rc<List>),
    Nil,
}

use crate::List::{Cons, Nil};
use std::rc::Rc;

fn main() {
    let a = Rc::new(Cons(1, Rc::new(Cons(2, Rc::new(Cons(3, Rc::new(Nil)))))));
    println!("count after creating a: {}", Rc::strong_count(&a));
    let _b = Cons(10, Rc::clone(&a));
    println!("count after creating b: {}", Rc::strong_count(&a));
    {
        let _c = Cons(5, Rc::clone(&a));
        println!("counter after creating c: {}", Rc::strong_count(&a));
    }
    println!("counter after drop c: {}", Rc::strong_count(&a));

    let five = Rc::new(5);
    println!("{}", five);
    println!(
        "the number of references of five: {}",
        Rc::strong_count(&five)
    );
}
