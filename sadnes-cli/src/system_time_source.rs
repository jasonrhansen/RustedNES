use sadnes_core::time_source::TimeSource;

use time;

pub struct SystemTimeSource {}

impl TimeSource for SystemTimeSource {
    fn time_ns(&self) -> u64 {
        time::precise_time_ns()
    }
}
