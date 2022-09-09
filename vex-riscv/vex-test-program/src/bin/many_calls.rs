#![no_std]
#![no_main]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use riscv_rt::entry;

extern crate panic_halt;

const ADDR: *mut u8 = 0x7000_0000 as *mut u8;

#[entry]
fn main() -> ! {
    unsafe {
        ADDR.write_volatile(b'a');
        ADDR.write_volatile(b'b');
        ADDR.write_volatile(b'\n');
    }

    loop {
        continue;
    }
}
