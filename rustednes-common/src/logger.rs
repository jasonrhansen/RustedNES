use clap_verbosity_flag::{LogLevel, Verbosity};
use tracing_subscriber::EnvFilter;

pub fn initialize<L>(verbosity: &Verbosity<L>)
where
    L: LogLevel,
{
    if verbosity.is_silent() {
        return;
    }

    if std::env::var("RUST_LOG").is_err() {
        std::env::set_var(
            "RUST_LOG",
            verbosity
                .log_level()
                .map(|level| level.as_str().to_lowercase())
                .unwrap_or_else(|| "trace".to_owned()),
        )
    }

    let format = tracing_subscriber::fmt::format()
        .without_time()
        .with_target(false)
        .with_level(false)
        .compact();

    tracing_subscriber::fmt::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .event_format(format)
        .init();
}
