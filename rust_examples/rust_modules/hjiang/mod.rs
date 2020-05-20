mod inaccessible;
pub mod nested;

pub fn function() {
    println!("called `hjiang::function()`");
}

fn private_function() {
    println!("called `hjiang::private_function()`")
}

pub fn indirect_access() {
    println!("called `hjiang::indirect_access()`, that\n> ");
    private_function();
}
