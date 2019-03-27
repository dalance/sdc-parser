use std::env;
use std::fs::File;
use std::io::Read;

fn main() {
    for (i, arg) in env::args().enumerate() {
        if i > 0 {
            let mut f = File::open(&arg).unwrap();
            let mut buf = String::new();
            let _ = f.read_to_string(&mut buf);

            let ret = sdc_parser::parse(buf.as_str());

            let _ = dbg!(ret);
        }
    }
}
