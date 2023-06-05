// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use derive_more::From;
use object::{elf, ObjectSegment};
use object::{Architecture, Endianness, Object, ObjectKind, SegmentFlags};

#[derive(From, Debug)]
pub enum Error {
    ObjectParseError(object::Error),
    UnexpectedArchitecture {
        found: Architecture,
        expected: Architecture,
    },
    UnexpectedEndian {
        found: Endianness,
        expected: Endianness,
    },
    UnexpectedSystemBitSize {
        found: u64,
        expected: u64,
    },
    UnexpectedElfKind {
        found: ObjectKind,
        expected: ObjectKind,
    },

    EntryPointNot32Bit {
        entry: u64,
    },
}

struct SegmentInfo<'a> {
    is_exec: bool,
    address: u32,
    data: &'a [u8],
    padding: u32,
}

fn stream_structure(contents: &[u8]) -> Result<(u32, Vec<SegmentInfo<'_>>), Error> {
    let file = object::File::parse(contents)?;

    if file.architecture() != Architecture::Riscv32 {
        return Err(Error::UnexpectedArchitecture {
            found: file.architecture(),
            expected: Architecture::Riscv32,
        });
    }

    if file.kind() != ObjectKind::Executable {
        return Err(Error::UnexpectedElfKind {
            found: file.kind(),
            expected: ObjectKind::Executable,
        });
    }

    if file.is_64() {
        return Err(Error::UnexpectedSystemBitSize {
            found: 64,
            expected: 32,
        });
    }

    if !file.is_little_endian() {
        return Err(Error::UnexpectedEndian {
            found: Endianness::Big,
            expected: Endianness::Little,
        });
    }

    let Ok(entry) = u32::try_from(file.entry()) else {
        return Err(Error::EntryPointNot32Bit { entry: file.entry() })
    };

    let mut segs = vec![];

    for seg in file.segments() {
        let SegmentFlags::Elf { p_flags } = seg.flags() else { continue };

        const TEXT_FLAGS: u32 = elf::PF_X | elf::PF_R;
        const DATA_FLAGS: u32 = elf::PF_R | elf::PF_W;
        const RODATA_FLAGS: u32 = elf::PF_R;
        let is_exec = match p_flags {
            TEXT_FLAGS => true,
            DATA_FLAGS => false,
            RODATA_FLAGS => false,
            _ => continue,
        };

        let addr = u32::try_from(seg.address()).expect("Malformed 32bit ELF");

        let mem_size = seg.size() as u32;
        let data = seg.data().expect("Malformed ELF");

        let file_size = data.len() as u32;
        let padding = mem_size - file_size;

        segs.push(SegmentInfo {
            is_exec,
            address: addr,
            data,
            padding,
        });
    }

    Ok((entry, segs))
}

pub fn stream(elf_contents: &[u8]) -> Result<Vec<u8>, Error> {
    use std::io::Write;

    let (entry, segs) = stream_structure(elf_contents)?;

    let stream_size = 4 + // entry
        4 + // num segments

        segs.iter().map(|seg|
            1 +              // is executable
            4 +              // address
            4 +              // data length
            seg.data.len() + // actual contents
            4                // padding
        ).sum::<usize>();

    let mut stream = Vec::with_capacity(stream_size);

    // unwraps are fine here because `write_all` for `Vec<_>` can
    // never return `Err(_)`

    // program header
    stream.write_all(&entry.to_le_bytes()).unwrap();
    stream
        .write_all(&(segs.len() as u32).to_le_bytes())
        .unwrap();

    // segment headers and data
    for seg in segs {
        stream
            .write_all(&[if seg.is_exec { 1 } else { 0 }])
            .unwrap();

        stream.write_all(&seg.address.to_le_bytes()).unwrap();
        stream
            .write_all(&(seg.data.len() as u32).to_le_bytes())
            .unwrap();

        stream.write_all(&seg.padding.to_le_bytes()).unwrap();
        stream.write_all(seg.data).unwrap();
    }

    Ok(stream)
}
