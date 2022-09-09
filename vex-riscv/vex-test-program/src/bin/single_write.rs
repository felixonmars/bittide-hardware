#![no_std]
#![no_main]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

/*

This program has an invalid instruction memory read at 80000C16.

The instruction memory range ends at 80000C14.

*/

use riscv_rt::entry;

extern crate panic_halt;

use core::fmt::Write;
use heapless::String;

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
    let i = 14;
    let mut s = String::<16>::new();
    let _ = write!(s, "Hey! {}\n", i);
    print(&s);

    loop {
        continue;
    }
}
