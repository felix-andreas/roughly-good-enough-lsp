use console::style;

pub fn warning(message: &str) {
    eprintln!(
        "{} {}",
        style("warning:").yellow().bold(),
        style(message).bold(),
    );
}

pub fn error(message: &str) {
    eprintln!("{} {}", style("error").red().bold(), style(message).bold(),);
}
