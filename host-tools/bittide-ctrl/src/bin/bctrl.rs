// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use std::{
    io::{Read, Write},
    path::PathBuf,
};

use bittide_ctrl::program_stream;
use clap::{CommandFactory, Parser};

#[derive(Parser, Debug)]
struct Args {
    #[arg(long)]
    boot_program: PathBuf,

    #[arg(long)]
    serial_port: Option<PathBuf>,

    #[arg(long)]
    baud: Option<u32>,

    /// Path to a named pipe which transmits
    /// the remote's output.
    #[arg(long)]
    remote_pipe_output: Option<PathBuf>,

    /// Path to a named pipe which receives
    /// the remote's input.
    #[arg(long)]
    remote_pipe_input: Option<PathBuf>,
}

trait RemoteConnection {
    fn write(&mut self, b: u8);

    fn read_non_blocking(&mut self) -> Option<u8>;

    fn read_blocking(&mut self) -> u8;
}

struct LocalInOut {
    stdout: std::io::StdoutLock<'static>,
    read_rx: std::sync::mpsc::Receiver<u8>,
    _thread_handle: std::thread::JoinHandle<()>,
}

impl LocalInOut {
    fn new() -> Self {
        let (tx, rx) = std::sync::mpsc::channel();

        let handle = std::thread::spawn(move || {
            let mut stdin = std::io::stdin().lock();

            let mut buf = [0];

            loop {
                match stdin.read_exact(&mut buf) {
                    Ok(_) => tx.send(buf[0]).expect("main thread terminated?"),
                    Err(_) => return,
                }
            }
        });

        Self {
            stdout: std::io::stdout().lock(),
            read_rx: rx,
            _thread_handle: handle,
        }
    }

    fn write(&mut self, b: u8) {
        self.stdout.write_all(&[b]).expect("stdout writing error");
        self.stdout.flush().expect("flush failed");
    }

    fn read_non_blocking(&mut self) -> Option<u8> {
        match self.read_rx.try_recv() {
            Ok(b) => Some(b),
            Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                panic!("stdin thread disconnected")
            }
            Err(_) => None,
        }
    }

    fn _read_blocking(&mut self) -> u8 {
        match self.read_rx.recv() {
            Ok(b) => b,
            Err(_) => panic!("stdin thread disconnected"),
        }
    }
}

struct RemotePipes {
    remote_in: std::fs::File,

    remote_out_rx: std::sync::mpsc::Receiver<u8>,
    _thread_handle: std::thread::JoinHandle<()>,
}

impl RemotePipes {
    fn new(remote_in_local_out: std::fs::File, mut remote_out_local_in: std::fs::File) -> Self {
        let (tx, rx) = std::sync::mpsc::channel();

        let handle = std::thread::spawn(move || {
            let mut buf = [0];

            loop {
                match remote_out_local_in.read_exact(&mut buf) {
                    Ok(_) => tx.send(buf[0]).expect("main thread terminated?"),
                    Err(_) => return,
                }
            }
        });

        Self {
            remote_in: remote_in_local_out,
            remote_out_rx: rx,
            _thread_handle: handle,
        }
    }
}

impl RemoteConnection for RemotePipes {
    fn write(&mut self, b: u8) {
        self.remote_in
            .write_all(&[b])
            .expect("stdout writing error");
        self.remote_in.flush().expect("flush failed");
    }

    fn read_non_blocking(&mut self) -> Option<u8> {
        match self.remote_out_rx.try_recv() {
            Ok(b) => Some(b),
            Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                panic!("remote out pipe thread disconnected")
            }
            Err(_) => None,
        }
    }

    fn read_blocking(&mut self) -> u8 {
        match self.remote_out_rx.recv() {
            Ok(b) => b,
            Err(_) => panic!("remote out pipe thread disconnected"),
        }
    }
}

impl RemoteConnection for Box<dyn serialport::SerialPort> {
    fn write(&mut self, b: u8) {
        self.write_all(&[b]).expect("serial port writing failed");
        self.flush().expect("flush failed");
    }

    fn read_non_blocking(&mut self) -> Option<u8> {
        let n = self.bytes_to_read().expect("serial port read failed");
        if n > 0 {
            let mut buf = [0];
            self.read_exact(&mut buf)
                .expect("serial port reported read data available, read failed");
            Some(buf[0])
        } else {
            None
        }
    }

    fn read_blocking(&mut self) -> u8 {
        let mut buf = [0];
        self.read_exact(&mut buf).expect("");
        buf[0]
    }
}

fn main() {
    let args = Args::parse();

    enum ExecMode {
        Serial {
            path: PathBuf,
            baud: u32,
        },
        Piped {
            remote_out: PathBuf,
            remote_in: PathBuf,
        },
    }

    let mode = match (
        args.serial_port,
        args.baud,
        args.remote_pipe_output,
        args.remote_pipe_input,
    ) {
        (Some(path), Some(baud), None, None) => ExecMode::Serial { path, baud },
        (None, None, Some(remote_out), Some(remote_in)) => ExecMode::Piped {
            remote_out,
            remote_in,
        },

        (Some(_), None, _, _) => {
            let mut cmd = Args::command();
            cmd.error(
                clap::error::ErrorKind::MissingRequiredArgument,
                "When `--serial-port` is given, also `--baud` needs to be given",
            )
            .exit()
        }
        (None, Some(_), _, _) => {
            let mut cmd = Args::command();
            cmd.error(
                clap::error::ErrorKind::MissingRequiredArgument,
                "`--baud` is only valid when `--serial-port` is given",
            )
            .exit()
        }

        (_, _, Some(_), None) => {
            let mut cmd = Args::command();
            cmd.error(
                clap::error::ErrorKind::MissingRequiredArgument,
                "`--remote-pipe-output` is only valid when `--remote-pipe-input` is also given",
            )
            .exit()
        }
        (_, _, None, Some(_)) => {
            let mut cmd = Args::command();
            cmd.error(
                clap::error::ErrorKind::MissingRequiredArgument,
                "`--remote-pipe-input` is only valid when `--remote-pipe-output` is also given",
            )
            .exit()
        }

        (None, None, None, None) => {
            let mut cmd = Args::command();
            cmd.error(
                clap::error::ErrorKind::MissingRequiredArgument,
                "Either `--serial-port` and `--baud` or `--remote-pipe-output` and `--remote-pipe-input` must be given",
            )
            .exit()
        }
        (Some(_), Some(_), Some(_), Some(_)) => {
            let mut cmd = Args::command();
            cmd.error(
                clap::error::ErrorKind::ArgumentConflict,
                "Either `--serial-port` and `--baud` or `--remote-pipe-output` and `--remote-pipe-input` must be given, not both!",
            )
            .exit()
        }
    };

    let boot_prog = std::fs::read(&args.boot_program).expect("Unable to open boot-program");

    let stream = program_stream::stream(&boot_prog).expect("Unable to create program stream");

    let mut local = LocalInOut::new();

    match mode {
        ExecMode::Serial { path, baud } => {
            let mut serial = serialport::new(path.display().to_string(), baud)
                .data_bits(serialport::DataBits::Eight)
                .flow_control(serialport::FlowControl::None)
                .parity(serialport::Parity::None)
                .open()
                .expect("unable to open serial port");
            run(&mut local, &mut serial, &stream)
        }
        ExecMode::Piped {
            remote_out,
            remote_in,
        } => {
            let remote_out =
                std::fs::File::open(remote_out).expect("Unable to open remote-out-local-in pipe");

            let remote_in =
                std::fs::File::open(remote_in).expect("Unable to open remote-in-local-out pipe");

            let mut piped = RemotePipes::new(remote_out, remote_in);

            run(&mut local, &mut piped, &stream)
        }
    }
}

const BOOT_UART: u8 = 0b0000_0001;
const ACK_BOOT_UART: u8 = 0b1000_0001;

fn run(local: &mut LocalInOut, remote_conn: &mut dyn RemoteConnection, stream: &[u8]) {
    // perform UART boot
    {
        remote_conn.write(BOOT_UART);

        let ack = remote_conn.read_blocking();

        if ack != ACK_BOOT_UART {
            eprintln!("remote did not acknowledge UART boot. Quitting.");
            return;
        }

        for b in stream {
            remote_conn.write(*b);
        }
    }

    // go into serial-console mode
    loop {
        if let Some(b) = remote_conn.read_non_blocking() {
            local.write(b);
        }
        if let Some(b) = local.read_non_blocking() {
            remote_conn.write(b);
        }
    }
}
