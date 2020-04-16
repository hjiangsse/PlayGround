use std::fmt::Display;

pub trait Summary {
    fn summary(&self) -> String;
    fn greeting(&self) -> String {
        format!("Hola amigo")
    }
}

pub struct NewsArticle {
    pub headline: String,
    pub location: String,
    pub author: String,
    pub content: String,
}

impl Summary for NewsArticle {
    fn summary(&self) -> String {
        format!("{}, by {} {}", self.headline, self.author, self.location)
    }
}

pub struct Tweet {
    pub username: String,
    pub content: String,
    pub reply: bool,
    pub retweet: bool,
}

impl Summary for Tweet {
    fn summary(&self) -> String {
        format!("{}: {}", self.username, self.content)
    }

    fn greeting(&self) -> String {
        format!("Hola amigo, from {}", self.username)
    }
}

pub fn notify(item: impl Summary) {
    println!("Breaking news: {}", item.summary())
}

fn largest<T: PartialOrd + Copy>(list: &[T]) -> T {
    let mut largest = list[0];

    for &item in list.iter() {
        if item > largest {
            largest = item;
        }
    }

    largest
}

fn ref_largest<T: PartialOrd>(list: &[T]) -> &T {
    let mut largest = &list[0];

    for item in list {
        if item > largest {
            largest = &item;
        }
    }

    largest
}

struct Pair<T> {
    x: T,
    y: T,
}

impl<T> Pair<T> {
    fn new(x: T, y: T) -> Self {
        Self { x, y }
    }
}

impl<T: Display + PartialOrd> Pair<T> {
    fn cmp_display(&self) {
        if self.x >= self.y {
            println!("The largest number is x = {}", self.x);
        } else {
            println!("The largest number is y = {}", self.y);
        }
    }
}

fn main() {
    let tweet = Tweet {
        username: String::from("hjiang"),
        content: String::from("rust traits is something like golang interface"),
        reply: false,
        retweet: false,
    };

    let news = NewsArticle {
        headline: String::from("the death of american coronvirous is soaring"),
        location: String::from("new york city"),
        author: String::from("hjiang"),
        content: String::from("unknown place"),
    };

    println!("the summary of the tweet: {}", tweet.summary());
    println!("the greeting from the tweet: {}", tweet.greeting());
    println!("the summary of the news artical: {}", news.summary());
    println!("the greeting from the news artical: {}", news.greeting());

    notify(tweet);
    notify(news);

    let number_list = vec![34, 50, 25, 100, 65];
    let largest_num = largest(&number_list);
    println!("largest number in list: {}", largest_num);

    let char_list = vec!['y', 'm', 'a', 'q'];
    let largest_char = largest(&char_list);
    println!("largest character in list: {}", largest_char);

    let refla = ref_largest(&number_list);
    println!("ref_largest number in list: {}", refla);

    let pair = Pair { x: 10, y: 20 };
    let new_pair = Pair::new(20, 30);
    pair.cmp_display();
    new_pair.cmp_display();
}
