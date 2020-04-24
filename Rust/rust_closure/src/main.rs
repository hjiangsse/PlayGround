use std::thread;
use std::time::Duration;

fn main() {
    let simulated_user_specified_value = 10;
    let simulated_random_number = 3;

    generate_workout(simulated_user_specified_value, simulated_random_number);

    let add_one_v1 = |num| num + 1;
    println!("{} add one is {}", 10, add_one_v1(10));

    let add_two_nums = |num1, num2| num1 + num2;
    println!("{} add {} is {}", 10, 20, add_two_nums(10, 20));

    let same_thing = |x| x;
    println!("{}", same_thing(String::from("test")));
}

struct Cacher<T>
where
    T: Fn(u32) -> u32,
{
    calculation: T,
    value: Option<u32>,
}

impl<T> Cacher<T>
where
    T: Fn(u32) -> u32,
{
    fn new(calculation: T) -> Cacher<T> {
        Cacher {
            calculation,
            value: None,
        }
    }

    fn value(&mut self, arg: u32) -> u32 {
        match self.value {
            Some(v) => v,
            None => {
                let v = (self.calculation)(arg);
                self.value = Some(v);
                v
            }
        }
    }
}

fn generate_workout(intensity: u32, random_number: u32) {
    /*
    let expensive_closure = |num| {
        println!("cal slowly...");
        thread::sleep(Duration::from_secs(2));
        num
    };
     */
    let mut expensive_result = Cacher::new(|num| {
        println!("cal slowly...");
        thread::sleep(Duration::from_secs(2));
        num
    });

    //let expensive_result = simulated_expensive_calculation(intensity);

    if intensity < 25 {
        println!(
            "Today, do {} pushups!",
            //simulated_expensive_calculation(intensity)
            //expensive_result
            //expensive_closure(intensity)
            expensive_result.value(intensity)
        );
        println!(
            "Next, do {} situps!",
            //simulated_expensive_calculation(intensity)
            //expensive_result
            //expensive_closure(intensity)
            expensive_result.value(intensity)
        );
    } else {
        if random_number == 3 {
            println!("Take a break today! Remember to stay hydrated!");
        } else {
            println!(
                "Today, run for {} minutes!",
                //simulated_expensive_calculation(intensity)
                //expensive_result
                //expensive_closure(intensity)
                expensive_result.value(intensity)
            );
        }
    }
}

/*
fn simulated_expensive_calculation(intensity: u32) -> u32 {
    println!("cal slowly...");
    thread::sleep(Duration::from_secs(2));
    intensity
}
*/
