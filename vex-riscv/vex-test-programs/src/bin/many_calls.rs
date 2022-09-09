#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use riscv_rt::entry;

#[cfg(not(test))]
extern crate panic_halt;

const ADDR: *mut u8 = 0x7000_0000 as *mut u8;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    unsafe {
        ADDR.write_volatile(b'H');
        ADDR.write_volatile(b'e');
        ADDR.write_volatile(b'y');
        ADDR.write_volatile(b'\n');
    }

    for i in 0..100u64 {
        unsafe {
            ADDR.write_volatile(b'a' + (i % 26) as u8);
        }
    }
    unsafe {
        ADDR.write_volatile(b'\n');
    }

    loop {
        continue;
    }
}
