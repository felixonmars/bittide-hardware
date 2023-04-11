#![no_std]
use core::fmt;
use core::fmt::Write;

#[derive(Debug)]
#[cfg_attr(feature = "defmt", derive(defmt::Format))]
pub struct Clock {
    global_time: u64,
    timer_last: u32,
    ticks_per_micro: u64,
    timer_now: *const u32,
}


// ...

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let elapsed = self.elapsed();
        let elapsed_millis = elapsed.millis();
        let hours = elapsed_millis / (60 * 60 * 1_000);
        let minutes = (elapsed_millis % (60 * 60 * 1_000)) / (60 * 1_000);
        let seconds = (elapsed_millis % (60 * 1_000)) / 1_000;
        let millis = elapsed_millis % 1_000;

        write!(
            f,
            "{:02}h:{:02}m:{:02}s.{:03}ms",
            hours, minutes, seconds, millis
        )
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Duration {
    micros: u64,
}

impl Duration {
    pub fn from_micros(micros: u64) -> Self {
        Duration { micros }
    }

    pub fn from_millis(millis: u64) -> Self {
        Duration {
            micros: millis * 1_000,
        }
    }

    pub fn micros(self) -> u64 {
        self.micros
    }

    pub fn millis(self) -> u64 {
        self.micros / 1_000
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Instant {
    micros: i64,
}

impl Instant {
    pub fn from_micros(micros: i64) -> Self {
        Instant { micros }
    }

    pub fn from_millis(millis: i64) -> Self {
        Instant {
            micros: millis * 1_000,
        }
    }

    pub fn micros(self) -> i64 {
        self.micros
    }

    pub fn millis(self) -> i64 {
        self.micros / 1_000
    }
}

impl Clock {
    pub fn new(addr: usize, frequency: u64) -> Clock {
        let timer_last = unsafe { (addr as *const u32).read_volatile() };

        Clock {
            global_time: 0,
            timer_last,
            ticks_per_micro: frequency / 10^6,
            timer_now: addr as *const u32,
        }
    }

    pub fn advance(&self, duration: Duration) {
        let now = self.get_ticks();
        let duration_ticks = duration.micros() * (self.ticks_per_micro);
        let target = now + (duration_ticks as u32);
        if target > now {
            let mut ticks = now;
            while ticks < target {
                ticks = self.get_ticks();
            }
        } else {
            let mut ticks = now;
            while ticks > now || ticks < target {
                ticks = self.get_ticks();
            }
        }
    }

    // Gets the current number of ticks since the Clock was created.
    pub fn elapsed_ticks(&mut self) -> u64 {
        unsafe {
            let now = self.get_ticks();
            self.global_time = self.global_time + (tick_diffs(self.timer_last, now) as u64);
            self.timer_last = now;
            self.global_time
        }
    }

    pub fn elapsed(&mut self) -> Instant {
        Instant::from_micros((self.elapsed_ticks() / self.ticks_per_micro) as i64)
    }

    // Gets the current current number of ticks in u32, this will overflow during runtime.
    fn get_ticks(&self) -> u32 {
        unsafe { self.timer_now.read_volatile() }
    }
}

pub fn tick_diffs(old: u32, new: u32) -> u32 {
    let mut ticks_since = 0;
    if old > new {
        ticks_since = !0 - old;
        ticks_since = ticks_since + new;
    } else {
        ticks_since = new - old;
    }
    ticks_since
}
