use clap::ColorChoice;
use yansi::Condition;

pub fn setup_panic_handler() {
    human_panic::setup_panic!(human_panic::Metadata::new(
        env!("CARGO_BIN_NAME"),
        env!("CARGO_PKG_VERSION")
    )
    .homepage("http://rgbds.gbdev.io")
    .authors(env!("CARGO_PKG_AUTHORS").replace(':', ", "))
    .support(
        "\
- Either open an issue on GitHub (if you have an account there):
  https://github.com/gbdev/rgbds/issues
  (Please search if there is a similar issue and comment on it, if possible.)
- Or contact one of the developers (see http://gbdev.io/chat).
  The preferred channel on Discord is #rgbds-dev."
    ));
}

pub fn detect_default_color_choice() {
    yansi::whenever(Condition::cached(
        Condition::os_support()
            && Condition::stderr_is_tty_live()
            && Condition::clicolor_live()
            && Condition::no_color_live(),
    ));
}

pub fn clap_color_choice() -> ColorChoice {
    // Always align `clap` on `yansi`, to avoid any discrepancies in how detection might be performed.
    if yansi::is_enabled() {
        ColorChoice::Always
    } else {
        ColorChoice::Never
    }
}

pub fn apply_color_choice(user_choice: ColorChoice) {
    match user_choice {
        ColorChoice::Auto => {} // Continue with the previous auto-detected result.
        ColorChoice::Always => yansi::enable(),
        ColorChoice::Never => yansi::disable(),
    }
}
