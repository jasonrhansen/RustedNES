use crate::time::TimeSource;

use time::OffsetDateTime;

pub struct SystemTimeSource {}

impl TimeSource for SystemTimeSource {
    fn time_ns(&self) -> u64 {
        (OffsetDateTime::now_utc() - OffsetDateTime::UNIX_EPOCH).whole_nanoseconds() as u64
    }
}
