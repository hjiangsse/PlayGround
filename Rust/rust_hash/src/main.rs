use std::collections::HashMap;

fn main() {
    //two method create hashmap in rust
    /*
    let mut scores = HashMap::new();
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 20);

    println!("now the hash is {:?}", scores);

    let students = vec![String::from("Jim"), String::from("Mary")];
    let students_num = vec![1, 2];

    let stu_hash: HashMap<_, _> = students.iter().zip(students_num.iter()).collect();
    println!("now the stu_hash is {:?}", stu_hash);

    let field_name = String::from("dog");
    let field_trait = String::from("bite");
    let mut dog_hash = HashMap::new();
    dog_hash.insert(field_name, field_trait);

    //println!("After insert the field_name is {}", field_name);
    let animal_name = String::from("cat");
    let animal_trait = String::from("catch mice");
    let mut cat_hash = HashMap::new();
    cat_hash.insert(&animal_name, &animal_trait);

    println!("After insert the animal_name is {}", animal_name);
    println!("After insert the animal_trait is {}", animal_trait);

    //get value using key in hashmap
    let key = String::from("dog");
    let val = dog_hash.get(&animal_name);

    if let Some(t) = val {
        println!("animal_trait: {}", t);
    } else {
        println!("can't locate {}", key);
    }
     */
    let name = "jim";
    let age = 20;
    let mut name_age_hash = HashMap::new();
    name_age_hash.insert(name, age);

    let key = "jim";
    let val = name_age_hash.get(&key);
    if let Some(v) = val {
        println!("find value: {}", v);
    } else {
        println!("No such key in hash map: {}", key);
    }

    //iterate through the hash map
    iter_hashmap(&name_age_hash);
    println!();
    name_age_hash.insert("jim", 33);
    println!("After another insert: ");
    iter_hashmap(&name_age_hash);

    name_age_hash.entry("lilei").or_insert(23);
    iter_hashmap(&name_age_hash);

    //change a old value
    let lilei_age = name_age_hash.entry("lilei").or_insert(0);
    *lilei_age = *lilei_age * 10;
    iter_hashmap(&name_age_hash);
}

fn iter_hashmap(mp: &std::collections::HashMap<&str, i32>) {
    for (k, v) in mp {
        println!("{} => {}", k, v);
    }
}
