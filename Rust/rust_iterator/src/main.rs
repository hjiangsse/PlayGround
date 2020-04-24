#[derive(PartialEq, Debug)]
struct Shoe {
    size: u32,
    style: String,
}

fn shoes_in_my_size(shoes: Vec<Shoe>, shoe_size: u32) -> Vec<Shoe> {
    shoes.into_iter().filter(|s| s.size == shoe_size).collect()
}

fn filters_by_size() {
    let shoes = vec![
        Shoe {
            size: 10,
            style: String::from("sneaker"),
        },
        Shoe {
            size: 13,
            style: String::from("sandal"),
        },
        Shoe {
            size: 10,
            style: String::from("boot"),
        },
    ];

    let in_my_size = shoes_in_my_size(shoes, 10);
    println!("{:?}", in_my_size);
}

struct Counter {
    count: u32,
}

impl Counter {
    fn new() -> Counter {
        Counter { count: 0 }
    }
}

impl Iterator for Counter {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        self.count + 1;

        if self.count < 6 {
            Some(self.count)
        } else {
            None
        }
    }
}

fn main() {
    filters_by_size();
    //  let mut counter = Counter::new();

    let vec1 = vec![1, 2, 3];
    let vec2 = vec![2, 3, 4];
    for i in vec1.iter().zip(&vec2).map(|(c, s)| c * s) {
        println!("{}", i)
    }

    /*
    let v1: Vec<i32> = vec![1, 2, 3];
    let v2: Vec<_> = v1.iter().map(|x| x + 1).collect();
    assert_eq!(v2, vec![2, 3, 4]);

    //    let total: u32 = v1_iter.sum();
    //  println!("{}", total);

    for val in v1_iter {
        println!("{}", val);
    }

    match v1_iter.next() {
        Some(&v) => println!("{}", v),
        None => println!("finish!"),
    }
    */
}

/*
fn walkiter<T>(iter: iter::Iterator) {
    match iter.Next() {
        Some(&v) => {
            println!("{}", v);
            walkiter(iter);
        }
        None => println!("finish!"),
    }
}
*/
