struct Student {
    name: String,
    number: i32,
}

impl Student {
    fn tell_number(&self) -> i32 {
        self.number
    }
}

fn main() {
    let stu = Student {
        name: String::from("hjiang"),
        number: 10,
    };

    println!("the number of stu: {}", stu.tell_number());
}
