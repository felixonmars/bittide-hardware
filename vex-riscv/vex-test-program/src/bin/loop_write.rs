#![no_std]
#![no_main]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

/*

This program causes an invalid data read at address 80000C00,
which is in the instruction memory.

When run in QEMU it accesses 80000C48, which is fine!

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
    for i in 0..50 {
        let mut s = String::<16>::new();
        let _ = write!(s, "Hey! {}\n", i);
        print(&s);
    }

    loop {
        continue;
    }
}
