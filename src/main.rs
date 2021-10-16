mod lex;




fn main() {
    println!("Hello, world!");
    let x = lex::Tokenizer{
        content_: String::from("123")
    };
}
