use std::env;
use std::fs::{self, File};
use std::io::Read;
use std::time::Instant;

fn main() {
    for (i, arg) in env::args().enumerate() {
        if i > 0 {
            let mut f = File::open(&arg).unwrap();
            let mut buf = String::new();
            let _ = f.read_to_string(&mut buf);

            let size = fs::metadata(&arg).unwrap().len();

            let start = Instant::now();

            let _ = sdc_parser::parse(buf.as_str());

            let time = start.elapsed();

            let rate = size as f64 / time.as_micros() as f64;

            println!(
                "{:10.0}KB\t{:?}\t{:10.2}MBps\t{}",
                size as f64 / 1024.0,
                time,
                rate,
                arg
            );
        }
    }
}
