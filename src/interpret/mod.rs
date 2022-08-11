
pub struct StackData {

}

pub enum LanguageObject {

}

pub struct LanguageJunctType {
    pub name: String
}

pub struct LanguageDisjunctType {
    pub name: String
}

enum Primitive {
    String(String),
    Nat(u64),
    Token(String) // mm hmm heheh
}