use std::mem::drop;
use std::ops::Deref;
use std::ops::DerefMut;

#[derive(Debug)]
struct Student {
    Name: String,
    Age: i32,
}

impl Student {
    fn new(str: String, age: i32) -> Student {
        Student {
            Name: str,
            Age: age,
        }
    }
}

struct MyBox<T>(T);

impl<T> MyBox<T> {
    fn new(x: T) -> MyBox<T> {
        MyBox(x)
    }
}

impl<T> Deref for MyBox<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> DerefMut for MyBox<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

struct CustomSmartPointer {
    data: String,
}

impl Drop for CustomSmartPointer {
    fn drop(&mut self) {
        println!("Dropping CustomSmartpointer with data `{}`!", self.data);
    }
}

enum List {
    Cons(i32, Box<List>),
    Nil,
}

use crate::List::{Cons, Nil};

fn main() {
    test_drop()
}

fn test_ref_count() {
    let a = Cons(5, Box::new(Cons(10, Box::new(Nil))));
    let b = Cons(3, Box::new(a));
    let c = Cons(10, Box::new(a));
}

fn test_drop() {
    let c = CustomSmartPointer {
        data: String::from("my stuff"),
    };
    let d = CustomSmartPointer {
        data: String::from("other stuff"),
    };

    drop(c);

    println!("CustomSmartPointer created!");
}

fn test_deref_and_derefmut() {
    let z = 12;
    let t = MyBox::new(z);
    assert_eq!(12, z);
    assert_eq!(12, *t);

    println!("{}", *t.deref());

    let mut boxstr = MyBox::new("this is a string");
    hello(&boxstr);

    *boxstr = "this is other string";
    hello(&boxstr);
}

fn test_box() {
    let hs = Box::new(Student::new(String::from("hjiang"), 30));
    println!("{:?}", hs);
    println!("{}", std::mem::size_of_val(&hs));

    let ss = Student::new(String::from("hjiang"), 30);
    println!("{:?}", ss);
    println!("{}", std::mem::size_of_val(&ss));

    let x = 5;
    let y = &x;

    println!("{}", x);
    println!("{}", *y);

    let w = 10;
    let q = Box::new(w);
    assert_eq!(10, w);
    assert_eq!(10, *q);
}

fn hello(name: &str) {
    println!("hello, {}!", name);
}
