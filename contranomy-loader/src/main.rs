#![no_std]
#![no_main]

extern crate contranomy_debug;

mod elf_validation;

use core::fmt::Write;
use elf_rs::ProgramHeaderFlags;
use elf_rs::{Elf32, ElfFile, ProgramType};
use elf_validation::ElfValidationToken;
use riscv_rt::entry;

use contranomy_debug::println;
use contranomy_debug::{character_device, dbg};

use crate::elf_validation::{validate_elf_file, ElfConfig, ElfValidationError};

static LOADED_PROGRAM: &[u8] =
    include_bytes!("../../target/riscv32imc-unknown-none-elf/release/contranomy-loaded-program");

struct LoaderConfig {
    instruction_memory_write_address: usize,
}

#[entry]
fn main() -> ! {
    unsafe {
        character_device::initialise(0x9000_0000);
    }

    println!("Reading ELF file...");

    let elf = Elf32::from_bytes(&LOADED_PROGRAM).expect("load elf file failed");

    let elf_config = ElfConfig {
        instruction_memory_address: 0x3000_0000..0x5000_0000,
        data_memory_address: 0x5000_0000..0x8000_0000,
    };

    let load_config = LoaderConfig {
        instruction_memory_write_address: 0x1000_0000,
    };

    let validation_token = match validate_elf_file(&elf, &elf_config) {
        Ok(tok) => {
            println!("ELF file validated!");
            tok
        }
        Err(ElfValidationError::SegmentOutOfRange {
            expected,
            found,
            segment_type,
        }) => {
            panic!(
                "ELF validation failed! Segment for {segment_type} out of range, \
                    expected {:#08X}..{:#08X} but found {:#08X}..{:#08X}. Aborting.",
                expected.start, expected.end, found.start, found.end,
            );
        }
    };

    println!("Loading ELF file...");
    unsafe {
        load_elf_file(validation_token, &elf, &elf_config, &load_config);
    }

    println!("Jumping to entry...");

    let entry_ptr = unsafe { entry_point_fn(&elf) };

    dbg!(entry_ptr);

    // A call is used here instead of a jump since the loaded program should not
    // terminate anyway and apart from one used up slock on the stack for the
    // return address there is functionally no difference.
    unsafe { entry_ptr() }
}

unsafe fn load_elf_file(
    _tok: ElfValidationToken, // used to make sure validation was run before loading
    elf: &impl ElfFile,
    elf_config: &ElfConfig,
    loader_config: &LoaderConfig,
) {
    for p in elf.program_header_iter() {
        // skip segments that don't need loading
        if p.ph_type() != ProgramType::LOAD {
            continue;
        }

        let paddr = usize::try_from(p.paddr()).unwrap();
        let size = usize::try_from(p.filesz()).unwrap();

        let addr = if p.flags().contains(ProgramHeaderFlags::EXECUTE) {
            let rel_offset = paddr - elf_config.instruction_memory_address.start;
            loader_config.instruction_memory_write_address + rel_offset
        } else {
            paddr
        };

        let addr_ptr = addr as *mut u8;

        // write the segment to its desired memory location

        // Depends on https://github.com/google-research/bittide/pull/32
        core::ptr::copy_nonoverlapping(p.content().as_ptr(), addr_ptr, size);
    }
}

unsafe fn entry_point_fn(elf: &impl ElfFile) -> unsafe extern "C" fn() -> ! {
    // the ELF entry point is a physical address, so no conversion needed
    let entry = usize::try_from(elf.entry_point()).unwrap();
    core::mem::transmute::<_, unsafe extern "C" fn() -> !>(entry)
}
