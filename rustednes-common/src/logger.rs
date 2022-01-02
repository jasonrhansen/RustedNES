use tracing_subscriber::EnvFilter;

pub fn initialize(verbosity: usize) {
    if std::env::var("RUST_LOG").is_err() {
        std::env::set_var(
            "RUST_LOG",
            match verbosity {
                0 => "warn",
                1 => "info",
                2 => "debug",
                _ => "trace",
            },
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
