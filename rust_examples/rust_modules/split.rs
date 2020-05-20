mod hjiang;

fn function() {
    println!("called `function()`");
}

fn main() {
    hjiang::function();
    function();
    hjiang::indirect_access();
    hjiang::nested::function();
}
