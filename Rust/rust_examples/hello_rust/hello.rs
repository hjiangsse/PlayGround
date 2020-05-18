use std::fmt;

///This is the main function
fn main() {
    println!("hello rust");
    println!("I'm a Rustacean!");

    println!("the 5th month has {} days!", 30);
    println!("{0}, this is {1}. {1}, this is {0}", "hjiang", "heng");

    //named args
    println!(
        "{good}, {bad} and {ugly}",
        good = "great programmer",
        bad = "bad programmer",
        ugly = "fool programmer"
    );

    //nspecial formatting
    println!(
        "{} of {:b} people know binary, the other half doesn't!",
        1, 2
    );
    println!("the low hex value of {} is {:x}", 100, 100);
    println!("the upper hex value of {} is {:X}", 100, 100);

    //width?
    println!("{number:>width$}", number = 100, width = 6); //right
    println!("{number:^width$}", number = 100, width = 6); //left

    println!("{number:>0width$}", number = 100, width = 6);
    println!("{number:^width$}", number = 100, width = 6);

    #[allow(dead_code)]
    #[derive(Debug)]
    struct Structure(i32);

    impl fmt::Display for Structure {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "({})", self.0)
        }
    }

    #[derive(Debug)]
    struct Deep(Structure);

    println!("This struct '{}' will be printed now...", Structure(3));

    println!("{:?} months in a year", 12);
    println!(
        "{1:?} {0:?} is the {actor:?} name.",
        "Slater",
        "Christain",
        actor = "actor's"
    );
    println!("{:#?}", Deep(Structure(10)));

    //implement a std::fmt::Display trait
    let origin = Point { x: 10, y: 20 };
    println!("the orgin point: {}", origin);

    //print decimal part selectively
    println!("Pi is roughly {:.3}", 3.14159);

    let complex = Complex {
        real: 3.3,
        imag: 7.2,
    };
    println!("{}", complex);
    println!("{:?}", complex);

    //testcase for List
    let v = List(vec![1, 2, 3]);
    println!("{}", v);

    //formatting
    for city in [
        City {
            name: "Dublin",
            lat: 53.347778,
            lon: -6.259722,
        },
        City {
            name: "Oslo",
            lat: 59.95,
            lon: 10.75,
        },
        City {
            name: "Vancouver",
            lat: 49.25,
            lon: -123.1,
        },
    ]
    .iter()
    {
        println!("{}", *city);
    }

    for color in [Color {
        red: 10,
        green: 10,
        blue: 10,
    }]
    .iter()
    {
        println!("{}", color)
    }
}

struct Point {
    x: i32,
    y: i32,
}

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}--{})", self.x, self.y)
    }
}

#[derive(Debug)]
struct Complex {
    real: f32,
    imag: f32,
}

impl fmt::Display for Complex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Display: {} + {}i", self.real, self.imag)
    }
}

struct List(Vec<i32>);

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let vec = &self.0;

        write!(f, "[")?;
        for (cnt, v) in vec.iter().enumerate() {
            if cnt != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", cnt, v)?;
        }
        write!(f, "]")
    }
}

struct City {
    name: &'static str,
    lat: f32,
    lon: f32,
}

impl fmt::Display for City {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let lat_c = if self.lat >= 0.0 { 'N' } else { 'S' };
        let lon_c = if self.lon >= 0.0 { 'E' } else { 'W' };

        write!(
            f,
            "{}: {:.3}°{} {:.3}°{}",
            self.name,
            self.lat.abs(),
            lat_c,
            self.lon.abs(),
            lon_c
        )
    }
}

#[derive(Debug)]
struct Color {
    red: u8,
    green: u8,
    blue: u8,
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "RGB ({}, {}, {}) 0x{:02x}{:02x}{:02x}",
            self.red, self.green, self.blue, self.red, self.green, self.blue
        )
    }
}
