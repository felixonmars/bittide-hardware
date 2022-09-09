#![no_std]
#![no_main]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

/*

This program just straight up segfaults ¯\_(ツ)_/¯

Interestingly enough, it only segfaults when three function calls are made

*/

use riscv_rt::entry;

extern crate panic_halt;

const ADDR: *mut u8 = 0x7000_0000 as *mut u8;

fn print(s: &str) {
    for b in s.bytes() {
        unsafe {
            ADDR.write_volatile(b);
        }
    }
}

#[entry]
fn main() -> ! {
    // print("0123456789012345678901234567890123456789012345678901234567890");
    print("0....|....1....|....2....|....3....|....4....|....5....|....6\n");
    print("0....|....1....|....2....|....3....|....4....|....5....|....6\n");
    print("0....|....1....|....2....|....3....|....4....|....5....|....6\n");

    loop {
        continue;
    }
}
