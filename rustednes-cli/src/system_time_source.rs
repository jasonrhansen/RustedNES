use rustednes_core::time_source::TimeSource;

use time::OffsetDateTime;

pub struct SystemTimeSource {}

impl TimeSource for SystemTimeSource {
    fn time_ns(&self) -> u64 {
        (OffsetDateTime::now_utc() - OffsetDateTime::unix_epoch()).whole_nanoseconds() as u64
    }
}
