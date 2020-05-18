mod inssort {
    use std::error;
    use std::fmt;

    pub fn insert(vec: &mut [u32], e: u32) -> Vec<u32> {
        let mut res = vec![];
        let pos = match vec.binary_search(&e) {
            Ok(_pos) => 0,
            Err(pos) => pos,
        };

        for i in 0..pos {
            res.push(vec[i]);
        }

        res.push(e);

        for i in pos..vec.len() {
            res.push(vec[i]);
        }

        return res;
    }

    pub fn search_sort(vec: &mut [u32]) -> Vec<u32> {
        let vec_len = vec.len();
        let mut sorted_start = vec![vec[0]];
        for i in 1..vec_len {
            let sorted_part = insert(&mut sorted_start, vec[i]);
            sorted_start = sorted_part;
        }
        return sorted_start;
    }

    #[derive(Debug, Clone)]
    pub struct NotFound;

    impl fmt::Display for NotFound {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "not find the element")
        }
    }

    impl error::Error for NotFound {
        fn source(&self) -> Option<&(dyn error::Error + 'static)> {
            None
        }
    }

    pub fn rust_binary_search(
        vec: &[u32],
        start: usize,
        end: usize,
        e: u32,
    ) -> Result<usize, NotFound> {
        if start > end {
            return Err(NotFound);
        }

        let mid = (start + end) / 2;

        if vec[mid] == e {
            Ok(mid)
        } else if vec[mid] < e {
            rust_binary_search(vec, mid + 1, end, e)
        } else {
            rust_binary_search(vec, start, mid - 1, e)
        }
    }
}

fn main() {
    let mut orig = vec![1, 3, 4, 5, 6];
    let finnally = inssort::insert(&mut orig, 2);
    println!("finnally: {:?}", finnally);

    let mut unsorted = vec![1, 3, 2, 5, 4];
    let res = inssort::search_sort(&mut unsorted);
    println!("the sorted res: {:?}", res);

    let sorted = vec![1, 2, 4, 5, 6];
    let search_res = inssort::rust_binary_search(&sorted, 0, sorted.len() - 1, 6);
    match search_res {
        Ok(pos) => println!("the position: {}", pos),
        Err(err) => println!("{}", err),
    }
}
