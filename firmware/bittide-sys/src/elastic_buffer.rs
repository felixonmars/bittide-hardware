use crc::{Crc, CRC_8_LTE};
// checksum the first 64 bytes
pub const N: usize = 64;

pub const BUFSZ: usize = 512; // easier for testing

// NB. this was selected to avoid clashes:
// https://github.com/facebook/zstd/blob/dev/doc/zstd_compression_format.md#zstandard-frames
pub const MAGIC_BYTES: [u8; 4] = [0xFD, 0x2F, 0xB5, 0x28];

pub fn find_magic_bytes(haystack: &[u8]) -> Option<usize> {
    enum FindState {
        NotMatch,
        FirstByte,
        SecondByte,
        ThirdByte,
    }

    let mut st = FindState::NotMatch;
    for (i, byte) in haystack.iter().enumerate() {
        match (byte, &st) {
            (0xFD, FindState::NotMatch) => {
                st = FindState::FirstByte;
            }
            (0x2F, FindState::FirstByte) => {
                st = FindState::SecondByte;
            }
            (0xB5, FindState::SecondByte) => {
                st = FindState::ThirdByte;
            }
            (0x28, FindState::ThirdByte) => {
                return Some(i);
            }
            _ => {
                st = FindState::NotMatch;
            }
        };
    }
    return None;
}

pub fn pick_begin(in_buf: &[u8; BUFSZ]) -> Option<usize> {
    const CRC8: Crc<u8> = Crc::<u8>::new(&CRC_8_LTE);

    // FIXME: think of min. size necessary, we naively double the buffer here.
    let mut find_buf: [u8; 2 * BUFSZ];
    // we have to initialize with 0 because Rust can't do pointer stuff like ATS
    find_buf = [0; 2 * BUFSZ];
    let fst_buf: &mut [u8] = &mut find_buf[0..BUFSZ];
    fst_buf.copy_from_slice(in_buf);
    let snd_buf: &mut [u8] = &mut find_buf[BUFSZ..2 * BUFSZ];
    snd_buf.copy_from_slice(in_buf);
    let res = find_magic_bytes(&find_buf);
    res.and_then(|ix| {
        let sample = &find_buf[ix + 4..ix + 4 + N];
        let chk = CRC8.checksum(sample);
        if find_buf[ix + 4 + N] == chk {
            Some(ix)
        } else {
            None
        }
    })
}
