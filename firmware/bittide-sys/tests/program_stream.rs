// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use proptest::prelude::*;
use test_strategy::proptest;

use bittide_sys::program_stream::*;

mod elf_common;
use elf_common::*;

unsafe fn write_program_to_memory(input: &mut impl Iterator<Item = u8>) -> Option<()> {
    let prog_hd = read_program_header(input)?;

    for _ in 0..prog_hd.num_segments {
        let seg_hd = read_segment_header(input)?;
        write_segment_data(&seg_hd, input)?;
        write_padding(&seg_hd);
    }

    Some(())
}

unsafe fn verify_program_contents(info: &ElfCreateInfo) {
    for seg in &info.segments {
        let addr = seg.addr as usize as *mut u8;

        let data_slice =
            std::slice::from_raw_parts(addr, seg.data.len() + seg.zero_padding as usize);

        assert_eq!(data_slice[0..seg.data.len()], seg.data);
        assert!(data_slice[seg.data.len()..].iter().all(|x| *x == 0));
    }
}

// Generate valid ELF files and convert them into a streaming format.
#[proptest(ProptestConfig { cases: 5000, max_shrink_iters: 1000, ..ProptestConfig::default() })]
fn all_elfs_can_be_converted_to_streaming(#[strategy(gen_valid_elf_file())] info: ElfCreateInfo) {
    let elf = create_elf_file(&info);
    bittide_ctrl::program_stream::stream(&elf).expect("ELF can be converted to stream");
}

// Generate valid ELF files and make sure that all streams are smaller than the ELF.
#[proptest(ProptestConfig { cases: 5000, max_shrink_iters: 1000, ..ProptestConfig::default() })]
fn all_streams_are_smaller_than_elfs(#[strategy(gen_valid_elf_file())] info: ElfCreateInfo) {
    let elf = create_elf_file(&info);
    let stream = bittide_ctrl::program_stream::stream(&elf).unwrap();
    assert!(elf.len() >= stream.len());
}

// Generate valid ELF files, convert them to a streaming format and make sure
// the segment data is loaded into the buffer properly.
#[proptest(ProptestConfig { cases: 5000, max_shrink_iters: 1000, ..ProptestConfig::default() })]
fn all_elfs_are_loaded_properly(#[strategy(gen_valid_elf_file())] mut info: ElfCreateInfo) {
    let buffer_size = elf_loaded_buffer_size(&info);

    unsafe {
        with_32bit_addr_buffer(u32::try_from(buffer_size).unwrap(), |buf| {
            let base_addr = buf.as_mut_ptr() as usize as u64;

            elf_info_set_base_addr(&mut info, base_addr);

            let elf = create_elf_file(&info);
            let stream = bittide_ctrl::program_stream::stream(&elf).unwrap();
            write_program_to_memory(&mut stream.into_iter()).unwrap();

            verify_program_contents(&info);
        });
    }
}
