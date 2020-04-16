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
*/

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

fn main() {
    /*
    let number_lst = vec![23, 11, 23, 34];
    let result = largest(&number_lst);
    println!("The largest number in number_lst: {}", largest(&number_lst));
     */
    let will_work = Point { x: 5, y: 10 };
    println!("the x coordinate is {}", will_work.x);
    println!("the y coordinate is {}", will_work.y);

    println!(
        "The x and y of will work: {} {}",
        will_work.x(),
        will_work.y()
    );

    let float_point = Point { x: 5.2, y: 10.3 };
    println!(
        "The distance of float point from original: {}",
        float_point.distance_from_origin()
    );

    let new_work = NewPoint { x: 5, y: 10.1 };
    println!("the x coordinate is {}", new_work.x);
    println!("the y coordinate is {}", new_work.y);

    let final_work = NewPoint { x: 10.2, y: "a" };
    let mixuped = new_work.mixup(final_work);
    println!("the x of mixup: {}", mixuped.x);
    println!("the y of mixup: {}", mixuped.y);
}
