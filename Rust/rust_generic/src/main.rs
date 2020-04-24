//use std::mem;

/*
fn largest(lst: &[i32]) -> i32 {
    let mut largest_num = lst[0];

    for &num in lst {
        if num > largest_num {
            largest_num = num;
        }
    }

    largest_num
}

fn largest<T>(lst: &[T]) -> T {
    let mut largest = lst[0];

    for &item in lst.iter() {
        if item > largest {
            largest = item
        }
    }

    largest
}


struct Point<T> {
    x: T,
    y: T,
}

impl<T> Point<T> {
    fn x(&self) -> &T {
        &self.x
    }

    fn y(&self) -> &T {
        &self.y
    }
}

//implement methods on a specific type
impl Point<f32> {
    fn distance_from_origin(&self) -> f32 {
        (self.x.powi(2) + self.y.powi(2)).sqrt()
    }
}

struct NewPoint<T, U> {
    x: T,
    y: U,
}

impl<T, U> NewPoint<T, U> {
    fn mixup<V, W>(self, other: NewPoint<V, W>) -> NewPoint<T, W> {
        NewPoint {
            x: self.x,
            y: other.y,
        }
    }
}

struct A;

struct Single(A);

struct SingleGen<T>(T);
*/

fn main() {}
