# sdc-parser
A parser library for Synopsys Design Constraints (sdc).

[![Build Status](https://dev.azure.com/dalance/sdc-parser/_apis/build/status/dalance.sdc-parser?branchName=master)](https://dev.azure.com/dalance/sdc-parser/_build/latest?definitionId=1&branchName=master)
[![Crates.io](https://img.shields.io/crates/v/sdc-parser.svg)](https://crates.io/crates/sdc-parser)
[![Docs.rs](https://docs.rs/sdc-parser/badge.svg)](https://docs.rs/sdc-parser)
[![codecov](https://codecov.io/gh/dalance/sdc-parser/branch/master/graph/badge.svg)](https://codecov.io/gh/dalance/sdc-parser)

## Usage

```Cargo.toml
[dependencies]
sdc_parser = "0.1.0"
```

## Example

```rust
use sdc_parser::{self, sdc};

fn main() {
    let result = sdc_parser::parse("current_instance duv");

    let expect = sdc::Sdc {
        commands: vec![sdc::Command::CurrentInstance(
            sdc::CurrentInstance {
                instance: Some(String::from("duv"))
            }
        )]
    };
    assert_eq!(expect, result);
}
```

## Benchmark

### Environment

- CPU: Xeon Gold 6134 @ 3.20GHz
- OS : CentOS Linux release 7.6.1810 (Core)

### Result

| Size[KB] | Time[ms] | Throughput[MBps] |
| -------- | -------- | ---------------- |
| 5617     | 470      | 12.22            |
| 11529    | 965      | 12.23            |
| 54059    | 2738     | 20.21            |
| 114528   | 9157     | 12.81            |
| 496898   | 30887    | 16.47            |

