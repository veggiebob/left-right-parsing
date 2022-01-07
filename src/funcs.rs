use std::ops::Add;

pub fn join<T: IntoIterator<Item=char>>(chars: T) -> String {
    chars.into_iter().fold(String::new(), |s, i| s.add(&*i.to_string()))
}

pub fn take(s: &String, n: usize) -> Option<(String, String)> {
    let chars = s.chars().collect::<Vec<_>>();
    if chars.len() < n {
        None
    } else {
        Some((join(chars[..n].to_vec()), join(chars[n..].to_vec())))
    }
}

pub fn char_at(s: &String, i: usize) -> Option<char> {
    let vec = s.chars().collect::<Vec<_>>();
    vec.get(i).cloned()
}

/// start inclusive, end exclusive
fn slice_string(s: &String, start: usize, end: usize) -> Option<String> {
    if end <= s.len() {
        Some(join(chars(s)[start..end].to_vec()))
    } else {
        None
    }
}

pub fn substring(s: &String, start: usize, length: usize) -> Option<String> {
    slice_string(s, start, start + length)
}

fn chars(s: &String) -> Vec<char> {
    s.chars().collect::<Vec<_>>()
}

pub fn take_while<F: FnMut(&char) -> bool>(s: &String, mut f: F) -> (String, String) {
    let mut i = 0;
    while let Some(c) = char_at(s, i) {
        if !f(&c) {
            break
        }
        i += 1;
    }
    let chars = chars(s);
    (
        join(chars[..i].to_vec()),
        join(chars[i..].to_vec())
    )
}

pub fn expect_str<E>(test: &String, phrase: &str, incorrect: E, too_short: E) -> Result<String, E> {
    if let Some((s, rest)) = take(test, phrase.len()) {
        if s == phrase.to_string() {
            Ok(rest)
        } else {
            Err(incorrect)
        }
    } else {
        Err(too_short)
    }
}
