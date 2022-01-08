#[macro_use] extern crate maplit;
use std::ops::Add;

pub mod parse;
pub mod lang_obj;
pub mod funcs;

#[cfg(test)]
pub mod test;

fn main() {
    println!("Hello, world!");
}
