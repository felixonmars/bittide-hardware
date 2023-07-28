#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use core::panic::PanicInfo;

#[cfg(not(test))]
use riscv_rt::entry;

// use ufmt::uwrite;

const STATUS_REG_ADDR: *mut u32 = 0xC000_0000 as *mut u32;

static mut BUF: u8 = 0;

fn write_buf(d: u8) {
    unsafe {
        (&mut BUF as *mut u8).write_volatile(d);
    }
}

fn read_buf() -> u8 {
    unsafe { (&BUF as *const u8).read_volatile() }
}

fn test_success() {
    unsafe {
        STATUS_REG_ADDR.write_volatile(1);
    }
}

fn test_failure() {
    unsafe {
        STATUS_REG_ADDR.write_volatile(2);
    }
}

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    write_buf(1);
    write_buf(3);
    write_buf(65);

    write_buf(read_buf() - 3);

    test_success();

    /*
    #[allow(clippy::eq_op)]
    {
        assert_eq!(14 + 51, 65);
    }

    for i in 0..5 {
        let mut s = heapless::String::<32>::new();
        _ = uwrite!(s, "{}", i);

        let num = s.parse::<i32>().unwrap();
        assert_eq!(i, num);
    }

    test_success();

    */
    loop {
        continue;
    }
}

#[panic_handler]
fn panic_handler(_info: &PanicInfo) -> ! {
    test_failure();
    loop {
        continue;
    }
}
