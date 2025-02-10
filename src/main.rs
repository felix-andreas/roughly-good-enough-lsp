use {
    clap::{Parser, Subcommand},
    roughly::{format, lsp},
    std::path::PathBuf,
};

#[tokio::main]
async fn main() {
    env_logger::init();

    match Cli::parse().command {
        None => {
            eprintln!("starting lsp server... (for more information run 'roughly --help')");
            lsp::run().await;
        }
        Some(command) => match command {
            Command::Fmt { files, help: _ } => format::run(&files),
            Command::Lint { help: _ } => todo!(),
            Command::Lsp { help: _ } => lsp::run().await,
        },
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
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Run the formatter on the given files or directories
    Fmt {
        /// R files to format.
        files: Vec<PathBuf>,
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
