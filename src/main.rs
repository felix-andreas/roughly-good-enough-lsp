use {
    clap::{Parser, Subcommand},
    roughly::{format, lsp},
    std::{path::PathBuf, process::ExitCode},
};

#[tokio::main]
async fn main() -> ExitCode {
    env_logger::init();

    let exit = match Cli::parse().command {
        None => {
            lsp::run().await;
            true
        }
        Some(command) => match command {
            Command::Fmt {
                files,
                check,
                diff,
                help: _,
            } => format::run(files.as_deref(), check, diff),
            Command::Lint { help: _ } => todo!(),
            Command::Lsp { help: _ } => {
                lsp::run().await;
                true
            }
        },
    };

    if exit {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}

#[derive(Parser)]
#[clap(disable_help_flag = true)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Option<Command>,
    #[clap(long, action = clap::ArgAction::HelpLong)]
    help: Option<bool>,
    /// Ignored ... here only to please VS Code
    #[clap(long, default_value_t = true)]
    stdio: bool,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Run the formatter on the given files or directories
    Fmt {
        /// R files to format.
        files: Option<Vec<PathBuf>>,
        /// Avoid writing any formatted files back; instead, exit with a non-zero status code if any files would have been modified, and zero otherwise
        #[clap(long, default_value_t = false)]
        check: bool,
        /// Avoid writing any formatted files back; instead, exit with a non-zero status code and the difference between the current file and how the formatted file would look like
        #[clap(long, default_value_t = false)]
        diff: bool,
        #[arg(long , action = clap::ArgAction::HelpLong)]
        help: Option<bool>,
    },
    /// Lint the given files or directories
    Lint {
        #[arg(long , action = clap::ArgAction::HelpLong)]
        help: Option<bool>,
    },
    /// Run the language server
    Lsp {
        #[arg(long , action = clap::ArgAction::HelpLong)]
        help: Option<bool>,
    },
}
