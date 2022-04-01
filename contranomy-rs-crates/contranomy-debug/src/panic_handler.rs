use core::fmt::Write;
use core::panic::PanicInfo;

#[inline(never)]
#[panic_handler]
pub fn panic(info: &PanicInfo) -> ! {
    let _ = writeln!(crate::character_device::CharacterDevice, "{}", info);

    loop {}
}
