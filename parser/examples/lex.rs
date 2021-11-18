use long_parser::lexer::{Error, Lexer};

fn main() {
    let mut l = Lexer::new(
        r#"
#include <longcc.h>
// #include <stdio.h>
// #include <stdlib.h>
#define __cplusplus 201103L
#include <iostream>
int main() {
    puts("hello");
}
"#,
    );
    let mut line = 0;
    loop {
        match l.next() {
            Ok(Some(t)) => {
                if t.loc().line() != line {
                    line = t.loc().line();
                    println!()
                }
                print!(
                    "{}{}",
                    if t.leading_space() { " " } else { "" },
                    t.kind().to_string()
                )
            }
            Ok(None) => break,
            Err(e) => match e.downcast_ref::<Error>() {
                Some(e) => panic!("{}", l.readable_error(e)),
                e => panic!("{:?}", e),
            },
        }
    }
}
