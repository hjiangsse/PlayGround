mod my_mod {
    fn private_function() {
        println!("called `my_pod::private_function()`");
    }

    pub fn function() {
        println!("called `my_pod::function()`");
    }

    //access other items in the same module
    pub fn indirect_access() {
        print!("called `my_mod::indirect_access()`, that\n> ");
        private_function();
    }

    //nested modules
    pub mod nested {
        pub fn function() {
            println!("called `my_mod::nested::function()`");
        }

        #[allow(dead_code)]
        fn private_function() {
            println!("called `my_pod::nested::private_function()`");
        }

        //functions declared using pub(in path) syntax are only visible
        //within the given path. `path` must be a parent or ancestor module
        pub(in crate::my_mod) fn public_function_in_my_mod() {
            print!("called `my_mod::nested::public_function_in_my_mod()`, that\n> ");
            public_function_in_nested();
        }

        pub(self) fn public_function_in_nested() {
            print!("called `my_mod::nested::public_function_in_nested()`");
        }

        pub(super) fn public_function_in_super_mod() {
            print!("called `my_mod::nested::public_function_in_super_mod()`");
        }
    }

    pub fn call_public_function_in_my_mod() {
        print!("called `my_mod::public_function_in_my_mod()`");
        nested::public_function_in_my_mod();
        print!("> ");
        nested::public_function_in_super_mod();
    }

    pub(crate) fn public_function_in_crate() {
        print!("called `my_mod::public_function_in_crate()`");
    }

    mod private_nested {
        #[allow(dead_code)]
        pub fn function() {
            print!("called `my_mod::private_nested::function()`");
        }

        #[allow(dead_code)]
        pub(crate) fn restricted_function() {
            print!("called `my_mod::private_nested::restricted_function()`");
        }
    }
}

fn function() {
    println!("called `function()`");
}

mod my {
    //a public struct with a public field of generic type 'T'
    #[derive(Debug)]
    pub struct OpenBox<T> {
        pub contents: T,
    }

    //a public struct with a private field of generic type 'T'
    #[derive(Debug)]
    pub struct CloseBox<T> {
        contents: T,
    }

    impl<T> CloseBox<T> {
        // a public construtor method
        pub fn new(contents: T) -> CloseBox<T> {
            CloseBox { contents: contents }
        }
    }
}

mod deeply {
    pub mod nested {
        pub fn function() {
            println!("called `deeply::nested::function()`");
        }
    }
}

use deeply::nested::function as deeply_function;

mod cool {
    pub fn function() {
        println!("called `cool::function()`");
    }
}

mod yours {
    fn function() {
        println!("called `yours::function()`");
    }

    mod cool {
        pub fn function() {
            println!("called `yours::cool::function()`");
        }
    }

    pub fn indirect_call() {
        print!("called `yours::indirect_call()`, that\n> ");

        self::function();
        function();

        self::cool::function();

        super::function();

        {
            use crate::cool::function as root_function;
            root_function();
        }
    }
}

fn main() {
    function();
    my_mod::function();

    my_mod::indirect_access();
    my_mod::nested::function();
    my_mod::call_public_function_in_my_mod();

    my_mod::public_function_in_crate();

    let open_box = my::OpenBox {
        contents: "this is a open box",
    };
    println!("the open_box is {:?}", open_box);

    let closed_box = my::CloseBox::new("This is a closed box");
    println!("the closed_box is {:?}", closed_box);

    deeply_function();

    yours::indirect_call();
}
