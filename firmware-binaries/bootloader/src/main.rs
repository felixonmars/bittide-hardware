#![no_std]
#![cfg_attr(not(test), no_main)]

// SPDX-FileCopyrightText: 2023 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use ufmt::uwriteln;

use bittide_sys::{
    program_stream::{self, MemoryConfiguration},
    uart::Uart,
};

#[cfg(not(test))]
use riscv_rt::entry;

const BOOT_UART: u8 = 0b0000_0001;
const ACK_BOOT_UART: u8 = 0b1000_0001;

// TODO: make sure the memory configuration is set up in a way
// that it agrees with all other parts of the system
const MEM_CONFIG: MemoryConfiguration = MemoryConfiguration {
    instruction_memory: 0x0010_0000..0x0011_0000,
    data_memory: 0x0011_0000..0x0012_0000,
};

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = unsafe { Uart::new(0x8000_0000 as *mut u8) };

    match uart.receive() {
        BOOT_UART => unsafe { boot_uart(&mut uart, &MEM_CONFIG) },
        _ => {
            uwriteln!(uart, "No boot mode selected, going into echo-mode").unwrap();

            loop {
                let b = uart.receive();
                uart.send(b);
            }
        }
    }
}

unsafe fn boot_uart(uart: &mut Uart, mem_config: &MemoryConfiguration) -> ! {
    // acknowledge UART boot start
    //
    // This acknowledgment is so that the host-part that sends the program
    // doesn't start sending the program stream too early and overflows
    // the UART FIFO, causing data loss
    uart.send(ACK_BOOT_UART);

    let mut stream = core::iter::from_fn(|| {
        // TODO: backpressure mechanism if needed?
        Some(uart.receive())
    });

    let header = program_stream::read_program_header(&mut stream).expect("stream end");

    if !header.is_valid(mem_config) {
        uwriteln!(
            uart,
            "program header from stream is not valid with the current memory configuration",
        )
        .unwrap();

        // we do a loop here because that avoids going through
        // the panic handler (if any, since panic=abort), which
        // just loops anyway.
        #[allow(clippy::empty_loop)]
        loop {}
    }

    for _ in 0..header.num_segments {
        let seg_header = program_stream::read_segment_header(&mut stream).expect("stream end");

        if !seg_header.is_valid(mem_config) {
            uwriteln!(
                uart,
                "segment header from stream is not valid with the current memory configuration",
            )
            .unwrap();

            // we do a loop here because that avoids going through
            // the panic handler (if any, since panic=abort), which
            // just loops anyway.
            #[allow(clippy::empty_loop)]
            loop {}
        }

        program_stream::write_segment_data(&seg_header, &mut stream).expect("stream end");
        program_stream::write_padding(&seg_header);
    }

    uwriteln!(
        uart,
        "program loaded, jumping to entry point 0x{:X}",
        header.entry
    )
    .unwrap();

    unsafe {
        let entry = header.entry;
        core::arch::asm! {
            "jr {0}",
            in(reg) entry,
        }
    }

    // This should not be reachable because of the unconditional jump
    // above.

    #[allow(clippy::empty_loop)]
    loop {}
}
