use std::fmt;

pub trait Summary {
    fn summary(&self) -> String;
}

pub struct NewsArticle {
    pub headline: String,
    pub location: String,
    pub author: String,
    pub content: String,
}

pub struct Tweet {
    pub username: String,
    pub content: String,
    pub reply: bool,
    pub retweet: bool,
}

impl Summary for NewsArticle {
    fn summary(&self) -> String {
        format!("{}, by {} at {}", self.headline, self.author, self.location)
    }
}

impl Summary for Tweet {
    fn summary(&self) -> String {
        format!("{}, by {}", self.content, self.username)
    }
}

impl fmt::Display for NewsArticle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({}, {}, {}, {})",
            self.headline, self.location, self.author, self.content
        )
    }
}

pub fn notify(item: &impl Summary) {
    println!("Breaking news: {}", item.summary())
}

pub fn new_notify<T: Summary>(item: &T) {
    println!("Breaking news: {}", item.summary())
}

pub fn summ_and_print<T: Summary + fmt::Display>(elem: &T) {
    println!("summary: {}", elem.summary());
    println!("display: {}", elem);
}

pub fn summ_and_print_where<T>(elem: &T)
where
    T: Summary + fmt::Display,
{
    println!("summary: {}", elem.summary());
    println!("display: {}", elem);
}

pub fn return_summarizable() -> impl Summary {
    NewsArticle {
        headline: String::from("the deadth number caused by corona is soaring!"),
        location: String::from("new york city"),
        author: String::from("hjiang"),
        content: String::from("make america great again!"),
    }
}

pub fn return_summarizable_new(switch: bool) -> impl Summary {
    if switch {
        NewsArticle {
            headline: String::from("the deadth number caused by corona is soaring!"),
            location: String::from("new york city"),
            author: String::from("hjiang"),
            content: String::from("make america great again!"),
        }
    } else {
        Tweet {
            username: String::from("hjiang"),
            content: String::from("this is the end of the world"),
            reply: false,
            retweet: false,
        }
    }
}

fn main() {
    let news = NewsArticle {
        headline: String::from("the deadth number caused by corona is soaring!"),
        location: String::from("new york city"),
        author: String::from("hjiang"),
        content: String::from("make america great again!"),
    };

    notify(&news);
    new_notify(&news);
    summ_and_print(&news);
    summ_and_print_where(&news);
    notify(&return_summarizable());
}
