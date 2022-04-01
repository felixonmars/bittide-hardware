use elf_rs::{ElfFile, ProgramHeaderFlags, ProgramType};

pub struct ElfValidationToken(());

pub struct ElfConfig {
    pub instruction_memory_address: core::ops::Range<usize>,
    pub data_memory_address: core::ops::Range<usize>,
}

pub enum ElfValidationError {
    SegmentOutOfRange {
        expected: core::ops::Range<usize>,
        found: core::ops::Range<usize>,
        segment_type: &'static str,
    },
}

pub fn validate_elf_file(
    elf: &impl ElfFile,
    config: &ElfConfig,
) -> Result<ElfValidationToken, ElfValidationError> {
    for p in elf.program_header_iter() {
        if p.ph_type() != ProgramType::LOAD {
            continue;
        }

        let addr = usize::try_from(p.paddr()).unwrap();
        let size = usize::try_from(p.memsz()).unwrap();
        let found_range = addr..(addr + size);

        if p.flags() == ProgramHeaderFlags::EXECUTE | ProgramHeaderFlags::READ {
            // instruction memory

            if config.instruction_memory_address.contains(&addr) {
                continue;
            } else {
                return Err(ElfValidationError::SegmentOutOfRange {
                    expected: config.instruction_memory_address.clone(),
                    found: found_range,
                    segment_type: "instruction memory",
                });
            }
        } else if p.flags() == ProgramHeaderFlags::READ
            || p.flags() == ProgramHeaderFlags::WRITE | ProgramHeaderFlags::READ
        {
            // data memory

            if config.data_memory_address.contains(&addr) {
                continue;
            } else {
                return Err(ElfValidationError::SegmentOutOfRange {
                    expected: config.data_memory_address.clone(),
                    found: found_range,
                    segment_type: "data memory",
                });
            }
        }
    }
    Ok(ElfValidationToken(()))
}
