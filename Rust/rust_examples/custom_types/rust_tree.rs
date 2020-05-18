use crate::Node::*;

enum Node {
    //a substantial exist node
    Cons(u32, Box<Node>, Box<Node>),
    //a nil node
    Nil,
}

impl Node {
    fn new(&self) -> Node {
        Nil
    }

    fn find_ins_place(&self, elem: u32) -> Node {
        match self {
            Cons(e, left, right) => {
                if *e == elem {
                    return Nil;
                } else if *e < elem {
                    return Self::find_ins_place(right, elem);
                } else {
                    return Self::find_ins_place(left, elem);
                }
            }
            Nil => Nil,
        }
    }
}

fn main() {}
