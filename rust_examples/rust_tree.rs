#[derive(Clone)]
pub enum Node {
    //a substantial exist node
    Cons(u32, Box<Node>, Box<Node>),
    //a nil node
    Nil,
}

impl Node {
    fn new() -> Node {
        Nil
    }

    // Tell if a node is Nil
    fn is_null_node(node: &Node) -> bool {
        match node {
            Cons(_e, _l, _r) => {
                return false;
            }
            Nil => {
                return true;
            }
        }
    }

    // Tell if a node is leaf node
    fn is_leaf_node(node: &Node) -> bool {
        match node {
            Cons(_e, l, r) => {
                if Self::is_null_node(l) && Self::is_null_node(r) {
                    return true;
                } else {
                    return false;
                }
            }
            Nil => {
                return false;
            }
        }
    }

    // give a element, find the insert place in the tree
    fn insert_elem(&mut self, elem: u32) {
        match self {
            Cons(e, left, right) => {
                if *e == elem {
                    //if insert element is the root node, stop insert
                    //the tree is not change
                    return;
                } else if *e < elem && Self::is_null_node(&**right) {
                    *right = Box::new(Node::Cons(elem, Box::new(Nil), Box::new(Nil)));
                } else if *e > elem && Self::is_null_node(&**left) {
                    *left = Box::new(Node::Cons(elem, Box::new(Nil), Box::new(Nil)));
                } else if *e < elem {
                    Self::insert_elem(right, elem);
                } else if *e > elem {
                    Self::insert_elem(left, elem);
                }
            }
            Nil => {
                *self = Node::Cons(elem, Box::new(Nil), Box::new(Nil));
            }
        }
    }

    //left first, right last
    fn traverse_tree(node: &Node) {
        match node {
            Nil => {}
            Cons(e, l, r) => {
                if !Self::is_null_node(&**l) {
                    Self::traverse_tree(l);
                }

                print!(" {} ", e);

                if !Self::is_null_node(&**r) {
                    Self::traverse_tree(r);
                }
            }
        }
    }
}

mod rebuild {
    use crate::Node;
    use crate::Node::*;
    use std::error;
    use std::fmt;

    #[derive(Debug, Clone)]
    pub struct LengthError;

    impl fmt::Display for LengthError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "the two lists have different length")
        }
    }

    impl error::Error for LengthError {
        fn source(&self) -> Option<&(dyn error::Error + 'static)> {
            None
        }
    }

    fn split_slice(orig: &[u32], pivot: u32) -> (&[u32], &[u32]) {
        let mut curr_index = 0;
        for n in orig {
            if *n == pivot {
                return (&orig[0..curr_index], &orig[(curr_index + 1)..]);
            } else {
                curr_index = curr_index + 1;
            }
        }
        return (orig, &[]);
    }

    pub fn rebuild_tree(preorder: &[u32], inorder: &[u32]) -> Result<Node, LengthError> {
        if preorder.len() != inorder.len() {
            return Err(LengthError);
        }

        if preorder.len() == 0 {
            return Ok(Node::new());
        } else {
            //get the sublist of preorder and inorder
            let pivot = preorder[0];
            let (inorder_left, inorder_right) = split_slice(inorder, pivot);

            let preorder_left = &preorder[1..inorder_left.len() + 1];
            let preorder_right = &preorder[inorder_left.len() + 1..];

            //first create a leaf node
            let mut tree = Node::new();
            tree.insert_elem(pivot);

            //rebuild left sub tree
            let left_sub_res = rebuild_tree(preorder_left, inorder_left);
            let left_sub_tree: Node;
            match left_sub_res {
                Ok(sub) => {
                    left_sub_tree = sub;
                }
                Err(err) => {
                    return Err(err);
                }
            }

            //rebuild right sub tree
            let right_sub_res = rebuild_tree(preorder_right, inorder_right);
            let right_sub_tree: Node;
            match right_sub_res {
                Ok(sub) => {
                    right_sub_tree = sub;
                }
                Err(err) => {
                    return Err(err);
                }
            }

            //merge current node, left sub tree, right sub tree, into the final tree
            match &mut tree {
                Cons(_e, left, right) => {
                    *left = Box::new(left_sub_tree);
                    *right = Box::new(right_sub_tree);
                }
                Nil => {}
            }

            return Ok(tree);
        }
    }
}

mod bstdel {
    use crate::Node;
    use crate::Node::*;

    // find minimum in a BST tree
    fn find_min(tree: &Node) -> u32 {
        match tree {
            Cons(e, left, _right) => {
                if Node::is_null_node(left) {
                    return *e;
                } else {
                    return find_min(left);
                }
            }
            Nil => {
                return 0;
            }
        }
    }

    //find the maximum in a BST tree
    #[allow(dead_code)]
    fn find_max(tree: &Node) -> u32 {
        match tree {
            Cons(e, _left, right) => {
                if Node::is_null_node(right) {
                    return *e;
                } else {
                    return find_max(right);
                }
            }
            Nil => {
                return 0;
            }
        }
    }

    //give a valid node, get the data out
    fn retrive_elem_from_node(node: &Node) -> u32 {
        match node {
            Cons(e, _, _) => {
                return *e;
            }
            Nil => {
                return 0;
            }
        }
    }

    //give a valid node, get the left node
    fn retrive_left_from_node(node: &Node) -> Node {
        match node {
            Cons(_, left, _) => {
                return *left.clone();
            }
            Nil => return node.clone(),
        }
    }

    //give a valid node, get the left node
    fn retrive_right_from_node(node: &Node) -> Node {
        match node {
            Cons(_, _, right) => {
                return *right.clone();
            }
            Nil => return node.clone(),
        }
    }

    //find the parent node of elem
    #[allow(dead_code)]
    fn find_parent_node(tree: &Node, elem: u32) -> &Node {
        match tree {
            Cons(e, left, right) => {
                if *e == elem {
                    //if elem in root node, no parent
                    return &Node::Nil;
                } else if !Node::is_null_node(left) && retrive_elem_from_node(left) == elem {
                    //left tree root is the elem
                    return tree;
                } else if !Node::is_null_node(right) && retrive_elem_from_node(right) == elem {
                    //right tree root is the elem
                    return tree;
                } else if *e < elem {
                    return find_parent_node(right, elem);
                } else {
                    return find_parent_node(left, elem);
                }
            }
            Nil => {
                return &Node::Nil;
            }
        }
    }

    //build bst from slice of numbers
    pub fn build_bst(elms: &[u32]) -> Node {
        let mut tree = Node::new();
        for e in elms {
            tree.insert_elem(*e);
        }
        return tree;
    }

    //delete an element from a bst tree
    //after delete, the bst remains a bst tree
    pub fn delete_elem_from_bst(bst: &Node, e: u32) -> Node {
        let k = retrive_elem_from_node(bst);
        let left = retrive_left_from_node(bst);
        let right = retrive_right_from_node(bst);
        let min_right = find_min(&right);

        if Node::is_null_node(bst) {
            return Node::Nil;
        } else if k > e {
            //delete element in the left tree
            let deleted_left_tree = delete_elem_from_bst(&left, e);
            return Node::Cons(k, Box::new(deleted_left_tree), Box::new(right));
        } else if k < e {
            //delete element in the right tree
            let deleted_right_tree = delete_elem_from_bst(&right, e);
            return Node::Cons(k, Box::new(left), Box::new(deleted_right_tree));
        } else if k == e && Node::is_null_node(&left) {
            //the current node will be deleted, and
            //the left child of this node is nil
            return right;
        } else if k == e && Node::is_null_node(&right) {
            //the current node will be deleted, and
            //the left right of this node is nil
            return left;
        } else {
            //the most sofiscated
            let delete_right_min = delete_elem_from_bst(&right, min_right);
            return Node::Cons(min_right, Box::new(left), Box::new(delete_right_min));
        }
    }
}

use crate::Node::*;

fn main() {
    let mut tree = Node::new();
    if Node::is_null_node(&tree) {
        println!("the tree is only a Nil node now!");
    }

    let leaf = Node::Cons(10, Box::new(Nil), Box::new(Nil));
    if Node::is_leaf_node(&leaf) {
        println!("this is the leaf node!");
    }

    tree.insert_elem(10);
    if Node::is_leaf_node(&tree) {
        println!("now tree change from Nil node to a Leaf Node");
    }

    tree.insert_elem(4);
    tree.insert_elem(5);
    if !Node::is_leaf_node(&tree) {
        println!("now tree change from leaf to a tree");
    }

    Node::traverse_tree(&tree);
    println!("-------------------------");

    let preorder_result = [1, 2, 4, 3, 5, 6];
    let inorder_result = [4, 2, 1, 5, 3, 6];
    let build_res = rebuild::rebuild_tree(&preorder_result, &inorder_result);
    match build_res {
        Ok(tree) => {
            Node::traverse_tree(&tree);
            println!();
        }
        Err(err) => {
            println!("rebuild tree error: {}", err);
        }
    }

    let mut bst = bstdel::build_bst(&[4, 2, 1, 5, 3, 6]);
    let deleted_bst = bstdel::delete_elem_from_bst(&mut bst, 4);
    println!("after root deleted: ");
    Node::traverse_tree(&deleted_bst);
}
