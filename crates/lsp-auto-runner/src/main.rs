use std::{
    io::Write,
    net::{Ipv4Addr, SocketAddr, TcpListener, TcpStream},
    os::fd::OwnedFd,
    process::{Command, Stdio},
};

use owo_colors::{OwoColorize, colors::xterm::Gray};

const PACKAGE: &str = "dt-tools-lsp";

fn main() -> std::io::Result<()> {
    let bind_addr = SocketAddr::new(Ipv4Addr::LOCALHOST.into(), 9257);
    let listener = TcpListener::bind(bind_addr)?;
    println!("{} {}", "Listening on".blue(), bind_addr.cyan());

    let mut args = std::env::args().skip(1);

    let cargo_args: Vec<_> = args.by_ref().take_while(|arg| arg != "--").collect();
    let program_args: Vec<_> = args.collect();

    loop {
        let (stream, _addr) = listener.accept()?;
        clear_screen_xterm();

        run_child(stream, &cargo_args, &program_args)?;
    }
}

fn run_child(
    stream: TcpStream,
    cargo_args: &[String],
    program_args: &[String],
) -> std::io::Result<()> {
    println!(
        "    {} {PACKAGE} {} {}",
        "Building".cyan().bold(),
        "(silent)".fg::<Gray>(),
        format_args!("(extra: {cargo_args:?})").fg::<Gray>()
    );

    let mut build_child = Command::new("cargo")
        .args(["build", "--message-format=json", "-p", PACKAGE])
        .args(cargo_args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;

    let jq_output = Command::new("jq")
        .args(["--join-output", r#"select(.reason == "compiler-artifact" and .profile.test == false) | .executable | select(. != null)"#])
        .stdin(Stdio::from(build_child.stdout.take().expect("should be piped")))
        .output()?;

    let output = build_child.wait_with_output()?;
    if !output.status.success() {
        println!(
            "{}",
            format_args!("Build was unsuccessful: {}\nChild stderr:", output.status).red()
        );
        std::io::stderr().write_all(&output.stderr)?;
    }

    let executable = str::from_utf8(&jq_output.stdout).expect("Invalid UTF-8 sequence");

    println!(
        "    {} {executable:?} \"stdio-ansi\" {program_args:?}",
        " Running".green().bold()
    );

    let stdin_fd: OwnedFd = stream.try_clone()?.into();
    let stdout_fd: OwnedFd = stream.into();

    let mut child = Command::new(executable)
        .arg("stdio-ansi")
        .args(program_args)
        .stdin(Stdio::from(stdin_fd))
        .stdout(Stdio::from(stdout_fd))
        .stderr(Stdio::inherit())
        .spawn()?;

    // Wait until the child dies. The child dies by itself when the stream ends.
    let status = child.wait()?;
    if !status.success() {
        println!("{}", format_args!("Child was unsuccessful: {status}").red());
    }

    Ok(())
}

fn clear_screen_xterm() {
    // \x1b[3J clears the scrollback buffer
    // \x1b[2J clears the screen
    // \x1b[1;1H moves the cursor to row 1, column 1
    print!("\x1b[3J\x1b[2J\x1b[1;1H");

    // Flush stdout to make sure it happens immediately
    let _ = std::io::stdout().flush();
}
