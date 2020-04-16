use std::collections::HashMap;
/*
fn get_nth_elem(v: &Vec<i32>, n: usize) -> i32 {
    match v.get(n) {
        Some(elem) => *elem,
        None => 0,
    }
}

#[derive(Debug)]
enum SpreadSheetCell {
    Int(i32),
    Float(f64),
    Text(String),
}
*/

fn find_average_of_vector(vec: &Vec<i32>) -> (i32, i32) {
    let mut num = 0;
    let mut sum = 0;
    for n in vec {
        sum = sum + n;
        num = num + 1;
    }
    (sum / num, sum % num)
}

fn find_median_of_vector(vec: &Vec<i32>) -> i32 {
    let mut inner_vec = vec.clone();
    inner_vec.sort();
    let median_index = inner_vec.len() / 2;
    inner_vec[median_index]
}

fn find_mode_of_vector(vec: &Vec<i32>) -> i32 {
    let mut num_time_hash = HashMap::new();
    for num in vec {
        let time = num_time_hash.entry(num).or_insert(0);
        *time += 1;
    }

    let mut mode = vec[0];
    let mut mode_num = 0;
    if let Some(t) = num_time_hash.get(&mode) {
        mode_num = *t;
    }

    for (k, v) in num_time_hash {
        if v > mode_num {
            mode = *k;
            mode_num = v;
        }
    }

    mode
}

fn main() {
    let vec = vec![1, 3, 3, 10, 2, 4, 6];
    let avg = find_average_of_vector(&vec);
    println!("avg is {} remain {}", avg.0, avg.1);

    let median = find_median_of_vector(&vec);
    println!("median is {}", median);

    let mode = find_mode_of_vector(&vec);
    println!("mode is {}", mode);
    /*
    let v1: Vec<i32> = Vec::new();
    let mut v2 = vec![1, 2, 3];
    println!("The data in v1: {:?}", v1);
    println!("The data in v2: {:?}", v2);

    //update a vector
    v2.push(4);
    v2.push(5);
    println!("The data in v2: {:?}", v2);

    //reading elems in vec
    let third: &i32 = &v2[2];
    println!("The third element is {}", third);

    //get return Option<&T>
    match v2.get(2) {
        Some(third) => println!("The third element is {}", third),
        None => println!("There is no third element."),
    }

    //let does_not_exist = &v2[100];
    let does_not_exist = v2.get(100);
    println!("{:?}", does_not_exist);

    println!("The third element of the vector: {}", get_nth_elem(&v2, 3));

    v2.push(6);
    let first = &v2[0];

    println!("The first element is: {}", first);

    //iterate over the vector
    for i in &v2 {
        println!("{}", i);
    }

    let mut v3 = v2;
    for i in &mut v3 {
        *i *= 10;
    }
    println!("now the vector change to:{:?}", v3);

    //use enum to store different types in a vector
    let mut row = vec![
        SpreadSheetCell::Int(3),
        SpreadSheetCell::Float(3.14),
        SpreadSheetCell::Text(String::from("Test")),
    ];
    for i in &mut row {
        match &*i {
            SpreadSheetCell::Int(n) => println!("Int value: {}", n),
            SpreadSheetCell::Float(f) => println!("Float value: {}", f),
            SpreadSheetCell::Text(t) => println!("String value: {}", t),
        }
    }

    println!("pop out the last element: {:?}", row.pop());
    */
}
