use std::cmp::max;
use std::collections::HashSet;
use std::hash::Hash;

/// O(n^2)
pub fn find_first_repeating_subsequence<T>(xs: &Vec<T>) -> Option<(Vec<T>, usize)>
    where T: PartialEq + Eq + Clone {
    for i in 0..xs.len() {
        let mut found_match = false;
        let mut match_index = 0;
        let mut offset = 0;
        for j in i+1..xs.len() {
            if found_match {
                if j - offset == match_index - 1 {
                    return Some((xs.as_slice()[i..match_index].to_vec(), i));
                }
                if xs[j - offset] != xs[j] {
                    break;
                }
            } else if xs[i] == xs[j] {
                found_match = true;
                match_index = j;
                offset = j - i;
            }
        }
    }
    None
}

/// O(n^3)
pub fn find_all_repeating_subsequences<T>(xs: &Vec<T>) -> HashSet<Vec<T>>
    where T: PartialEq + Eq + Clone + Hash {
    let mut xs = xs.clone();
    let mut seq = HashSet::new();
    while let Some((s, index)) = find_first_repeating_subsequence(&xs) {
        seq.insert(s);
        xs.drain(0..index+1);
    }
    seq
}

/// specifically, the number of *adjacent* repeats
/// O(n)
pub fn max_repeats<T>(xs: &Vec<T>, subseq: &Vec<T>) -> usize
    where T: PartialEq + Eq {
    let mut s_index = 0;
    let mut count = 0;
    let mut max_count = 0;
    for i in 0..xs.len() {
        if s_index >= subseq.len() {
            count += 1;
            s_index = 0;
        }
        if xs[i] != subseq[s_index] {
            max_count = max(count, max_count);
            count = 0;
            s_index = 0;
        }
        if xs[i] == subseq[s_index] {
            s_index += 1;
        }
    }
    if s_index >= subseq.len() {
        max_count = max(max_count, count + 1);
    }
    max_count
}

/// O(n^3) * O(n) = O(n^4)
pub fn maximum_repeated_subseq<T>(xs: &Vec<T>) -> usize
    where T: PartialEq + Eq + Clone + Hash {
    find_all_repeating_subsequences(xs).iter()
        .map(|seq| max_repeats(xs, seq))
        .max().unwrap_or(0)
}

mod test {
    use crate::ez_parse::cycles::{find_all_repeating_subsequences, find_first_repeating_subsequence, max_repeats, maximum_repeated_subseq};

    #[test]
    fn test_repeating_subseq() {
        let seq = vec![1, 2, 3, 1, 2, 3];
        assert_eq!(find_first_repeating_subsequence(&seq), Some((vec![1, 2, 3], 0)));
        println!("{:?}", find_all_repeating_subsequences(&seq));
        assert_eq!(max_repeats(&seq, &vec![1, 2, 3]), 2);

        let seq = vec![6, 7, 1, 3, 1, 3, 1, 3, 8, 9, 8, 9];
        assert_eq!(find_first_repeating_subsequence(&seq), Some((vec![1, 3], 2)));
        println!("{:?}", find_all_repeating_subsequences(&seq));
        assert_eq!(max_repeats(&seq, &vec![1, 3]), 3);
        println!("{}", maximum_repeated_subseq(&seq));
    }
}