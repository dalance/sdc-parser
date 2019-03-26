use combine::parser::Parser;
use combine::stream::state::State;
use sdc_parser::sdc_parser;
use std::env;
use std::fs::File;
use std::io::Read;

fn main() {
    for (i, arg) in env::args().enumerate() {
        if i > 0 {
            let mut f = File::open(&arg).unwrap();
            let mut buf = String::new();
            let _ = f.read_to_string(&mut buf);

            let mut parser = sdc_parser();
            let ret = parser.easy_parse(State::new(buf.as_str()));

            let _ = dbg!(ret);
        }
    }
}
