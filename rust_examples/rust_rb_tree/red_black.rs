#[derive(Copy, Clone)]
enum Color {
    Red,
    Black,
}

#[derive(Clone)]
enum Side {
    Left,
    Right,
    Nil,
}

//a red black tree node
#[derive(Clone)]
enum RedBlackNode {
    Cons(
        Color,
        u32,
        Box<RedBlackNode>,
        Box<RedBlackNode>,
        Box<RedBlackNode>,
    ),
    Nil,
}

impl RedBlackNode {
    #[allow(dead_code)]
    fn new(color: Color, data: u32) -> RedBlackNode {
        return RedBlackNode::Cons(
            color,
            data,
            Box::new(RedBlackNode::Nil),
            Box::new(RedBlackNode::Nil),
            Box::new(RedBlackNode::Nil),
        );
    }
}

// retrive a node from a red-black tree, return the reference to the Node
#[allow(dead_code)]
fn retrive_node_from_tree(tree: &RedBlackNode, x: u32) -> &RedBlackNode {
    match tree {
        RedBlackNode::Cons(_, e, left, right, _parent) => {
            if *e == x {
                return tree;
            } else if *e > x {
                return retrive_node_from_tree(left, x);
            } else {
                return retrive_node_from_tree(right, x);
            }
        }
        RedBlackNode::Nil => {
            return tree;
        }
    }
}

fn retrive_left_node(node: &RedBlackNode) -> &RedBlackNode {
    match node {
        RedBlackNode::Cons(_, _, left, _, _) => {
            return left;
        }
        RedBlackNode::Nil => {
            return node;
        }
    }
}

fn retrive_right_node(node: &RedBlackNode) -> &RedBlackNode {
    match node {
        RedBlackNode::Cons(_, _, _, right, _) => {
            return right;
        }
        RedBlackNode::Nil => {
            return node;
        }
    }
}

fn retrive_parent_node(node: &RedBlackNode) -> &RedBlackNode {
    match node {
        RedBlackNode::Cons(_, _, _, _, parent) => {
            return parent;
        }
        RedBlackNode::Nil => {
            return node;
        }
    }
}

fn retrive_node_data(node: &RedBlackNode) -> u32 {
    match node {
        RedBlackNode::Cons(_, e, _, _, _) => {
            return *e;
        }
        RedBlackNode::Nil => {
            return 0;
        }
    }
}

fn retrive_parent_side(node: &RedBlackNode) -> Side {
    let node_data = retrive_node_data(node);
    let parent = retrive_parent_node(node);

    match parent {
        RedBlackNode::Cons(_, _, left, _right, _) => {
            if retrive_node_data(left) == node_data {
                return Side::Left;
            } else {
                return Side::Right;
            }
        }
        RedBlackNode::Nil => {
            return Side::Nil;
        }
    }
}

fn set_left(node: &mut RedBlackNode, left: &RedBlackNode) {
    match node {
        RedBlackNode::Cons(_, _, lf, _, _) => {
            *lf = Box::new((*left).clone());
        }
        RedBlackNode::Nil => {}
    }
}

fn set_right(node: &mut RedBlackNode, right: &RedBlackNode) {
    match node {
        RedBlackNode::Cons(_, _, _, ri, _) => {
            *ri = Box::new((*right).clone());
        }
        RedBlackNode::Nil => {}
    }
}

fn set_parent(node: &mut RedBlackNode, parent: &RedBlackNode) {
    match node {
        RedBlackNode::Cons(_, _, _, _, pa) => {
            *pa = Box::new((*parent).clone());
        }
        RedBlackNode::Nil => {}
    }
}

//set left and right child
fn set_children(node: &mut RedBlackNode, left: &RedBlackNode, right: &RedBlackNode) {
    set_left(node, left);
    set_right(node, right);
}

//traverse a red-black tree in infix order
#[allow(dead_code)]
fn infix_trave(tree: &RedBlackNode) {
    match tree {
        RedBlackNode::Cons(c, e, left, right, _) => {
            if !tell_null_node(left) {
                infix_trave(left);
            }

            print!(
                " ({} {}) ",
                match c {
                    Color::Red => "r",
                    Color::Black => "b",
                },
                *e
            );

            if !tell_null_node(right) {
                infix_trave(right);
            }
        }
        RedBlackNode::Nil => {}
    }
}

//traverse a red-black tree in infix order
#[allow(dead_code)]
fn suffix_trave(tree: &RedBlackNode) {
    match tree {
        RedBlackNode::Cons(c, e, left, right, _) => {
            print!(
                " ({} {}) ",
                match c {
                    Color::Red => "r",
                    Color::Black => "b",
                },
                *e
            );

            if !tell_null_node(left) {
                suffix_trave(left);
            }

            if !tell_null_node(right) {
                suffix_trave(right);
            }
        }
        RedBlackNode::Nil => {}
    }
}

// tell if a red-black tree node is a Nil node
fn tell_null_node(node: &RedBlackNode) -> bool {
    match node {
        RedBlackNode::Nil => return true,
        _ => return false,
    }
}

// left rotate node, which node is x, right child of node is y
#[allow(dead_code)]
fn left_rotate(x: &RedBlackNode) -> RedBlackNode {
    let mut x_node_clone = (*x).clone();

    if tell_null_node(x) {
        return RedBlackNode::Nil;
    } else {
        let y = retrive_right_node(x);
        let mut y_node_clone = (*y).clone();

        let a = retrive_left_node(x);
        let b = retrive_left_node(y);
        let c = retrive_right_node(y);

        set_children(&mut x_node_clone, a, b);
        set_children(&mut y_node_clone, &x_node_clone, c);

        return y_node_clone;
    }
}

// left rotate node x in a tree
#[allow(dead_code)]
fn left_rotate_tree(tree: &mut RedBlackNode, x: u32) -> RedBlackNode {
    match tree {
        RedBlackNode::Cons(c, e, left, right, parent) => {
            if *e == x {
                return left_rotate(tree);
            } else if *e < x {
                return RedBlackNode::Cons(
                    *c,
                    *e,
                    (*left).clone(),
                    Box::new(left_rotate(right)),
                    (*parent).clone(),
                );
            } else {
                return RedBlackNode::Cons(
                    *c,
                    *e,
                    Box::new(left_rotate(left)),
                    (*right).clone(),
                    (*parent).clone(),
                );
            }
        }
        RedBlackNode::Nil => {
            return RedBlackNode::Nil;
        }
    }
}

// left rotate node, which node is x, right child of node is y
#[allow(dead_code)]
fn right_rotate(y: &RedBlackNode) -> RedBlackNode {
    let mut y_node_clone = (*y).clone();

    if tell_null_node(y) {
        return RedBlackNode::Nil;
    } else {
        let x = retrive_left_node(y);
        let mut x_node_clone = (*x).clone();

        let a = retrive_left_node(x);
        let b = retrive_right_node(x);
        let c = retrive_right_node(y);

        set_children(&mut y_node_clone, b, c);
        set_children(&mut x_node_clone, a, &y_node_clone);

        return x_node_clone;
    }
}

// left rotate node x in a tree
#[allow(dead_code)]
fn right_rotate_tree(tree: &mut RedBlackNode, x: u32) -> RedBlackNode {
    match tree {
        RedBlackNode::Cons(c, e, left, right, parent) => {
            if *e == x {
                return right_rotate(tree);
            } else if *e < x {
                return RedBlackNode::Cons(
                    *c,
                    *e,
                    (*left).clone(),
                    Box::new(right_rotate(right)),
                    (*parent).clone(),
                );
            } else {
                return RedBlackNode::Cons(
                    *c,
                    *e,
                    Box::new(right_rotate(left)),
                    (*right).clone(),
                    (*parent).clone(),
                );
            }
        }
        RedBlackNode::Nil => {
            return RedBlackNode::Nil;
        }
    }
}

fn main() {
    let mut root = RedBlackNode::new(Color::Black, 4);
    let mut root_left = RedBlackNode::new(Color::Red, 3);
    let mut root_right = RedBlackNode::new(Color::Red, 7);
    let mut second_left = RedBlackNode::new(Color::Red, 5);
    let mut second_right = RedBlackNode::new(Color::Red, 8);

    set_children(&mut root_right, &second_left, &second_right);
    set_parent(&mut second_left, &root_right);
    set_parent(&mut second_right, &root_right);

    set_children(&mut root, &root_left, &root_right);
    set_parent(&mut root_left, &root);
    set_parent(&mut root_right, &root);

    suffix_trave(&root);
    let side = retrive_parent_side(&mut root_left);
    match side {
        Side::Left => println!("left side"),
        Side::Right => println!("right side"),
        Side::Nil => println!("no parent"),
    }

    let rotated_tree = left_rotate(&root);
    suffix_trave(&rotated_tree);
    let mut recover = right_rotate(&rotated_tree);
    suffix_trave(&recover);

    //-------------------------------------------------
    let mut big_root = RedBlackNode::new(Color::Red, 15);
    let mut big_right = RedBlackNode::new(Color::Red, 19);
    set_children(&mut big_root, &recover, &big_right);
    set_parent(&mut recover, &big_root);
    set_parent(&mut big_right, &big_root);

    println!("\n*********************");
    suffix_trave(&big_root);
    println!();
    let left_rotate_big = left_rotate_tree(&mut big_root, 4);
    suffix_trave(&left_rotate_big);
    println!();
    //-------------------------------------------------
}
