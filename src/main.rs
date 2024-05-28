mod command_parser;
mod ppc_id;
mod symbol_parser;
mod usbgecko;

use command_parser::Parser;
use gdbstub::{
    common::Signal,
    conn::Connection,
    stub::{
        run_blocking::{BlockingEventLoop, Event, WaitForStopReasonError},
        DisconnectReason, GdbStub, GdbStubError, SingleThreadStopReason,
    },
};
use log::{debug, info, LevelFilter, SetLoggerError};
use simple_logger::SimpleLogger;
use std::{
    fmt::{Debug, Display}, io::ErrorKind, net::{SocketAddr, TcpStream}, num::ParseIntError, str::FromStr
};
use usbgecko::{BreakpointStatus, USBGeckoDevice, USBGeckoError};

use argparse::{ArgumentParser, StoreConst, StoreOption};

use crate::usbgecko::{UpdateFn, WiiStatus};

pub fn init_logger(level: LevelFilter) -> Result<(), SetLoggerError> {
    log::set_boxed_logger(Box::new(SimpleLogger::default())).map(|()| log::set_max_level(level))
}

#[derive(Debug)]
enum GeckoDBError<T, C> {
    IoError(std::io::Error),
    USBGeckoError(USBGeckoError),
    GDBStubError(GdbStubError<T, C>)
}

impl<T, C> Display for GeckoDBError<T, C>
where
    T: Display,
    C: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GeckoDBError::IoError(e) => write!(f, "Error with the network connection to the USBGecko: {}", e),
            GeckoDBError::USBGeckoError(e) => write!(f, "Error with the USBGecko device: {}", e),
            GeckoDBError::GDBStubError(e) => write!(f, "Error with the GDBStub: {}", e),
        }
    }
}

impl<T, C> std::error::Error for GeckoDBError<T, C>
where
    T: Debug + Display,
    C: Debug + Display,
{
}

impl<T, C> From<GdbStubError<T, C>> for GeckoDBError<T, C> {
    fn from(value: GdbStubError<T, C>) -> Self {
        GeckoDBError::GDBStubError(value)
    }
}

impl<T, C> From<USBGeckoError> for GeckoDBError<T, C> {
    fn from(value: USBGeckoError) -> Self {
        GeckoDBError::USBGeckoError(value)
    }
}

struct GlobalEventLoop {}

impl BlockingEventLoop for GlobalEventLoop {
    type Target = Parser;
    type Connection = TcpStream;
    type StopReason = SingleThreadStopReason<u32>;
    fn wait_for_stop_reason(
        target: &mut Parser,
        conn: &mut Self::Connection,
    ) -> Result<
        gdbstub::stub::run_blocking::Event<Self::StopReason>,
        gdbstub::stub::run_blocking::WaitForStopReasonError<
            <Self::Target as gdbstub::target::Target>::Error,
            <Self::Connection as Connection>::Error,
        >,
    > {
        debug!("wait_for_stop_reason()");
        loop {
            // Check if data was sent while waiting
            let mut buf: Vec<u8> = std::iter::repeat(0).take(1).collect::<Vec<u8>>();
            let read_count = conn
                .peek(&mut buf)
                .or_else(|err| Err(WaitForStopReasonError::Connection(err)))?;
            if read_count > 0 {
                return Ok(Event::IncomingData(buf[0]));
            }
            match target.get_device().breakpoint_hit() {
                BreakpointStatus::Hit => match target.get_device().status() {
                    Ok(WiiStatus::Breakpoint) => {
                        return Ok(Event::TargetStopped(SingleThreadStopReason::HwBreak(())))
                    }
                    Ok(WiiStatus::Paused) => {
                        return Ok(Event::TargetStopped(SingleThreadStopReason::SwBreak(())))
                    }
                    _ => {
                        return Ok(Event::TargetStopped(SingleThreadStopReason::Signal(
                            Signal::UNKNOWN,
                        )))
                    }
                },
                BreakpointStatus::NotHit | BreakpointStatus::NoReply => continue,
            }
        }
    }

    fn on_interrupt(
        target: &mut Self::Target,
    ) -> Result<Option<Self::StopReason>, <Parser as gdbstub::target::Target>::Error> {
        debug!("User Interupted!");
        target.interrupt()?;
        Ok(Some(SingleThreadStopReason::Signal(Signal::SIGINT)))
    }
}

struct GdbServer<'a> {
    stub: GdbStub<'a, Parser, TcpStream>,
    parser: &'a mut Parser,
}

impl<'a> GdbServer<'a> {
    fn new(c: TcpStream, parser: &'a mut Parser) -> Self {
        Self {
            stub: GdbStub::new(c),
            parser,
        }
    }

    fn run(
        self,
    ) -> Result<DisconnectReason, GdbStubError<USBGeckoError, <TcpStream as Connection>::Error>>
    {
        self.stub.run_blocking::<GlobalEventLoop>(self.parser)
    }
}

fn run_server(
    parser: &mut Parser,
) -> Result<(), GeckoDBError<USBGeckoError, <TcpStream as Connection>::Error>> {
    let port: u16 = 2159;
    let listener = std::net::TcpListener::bind(format!("0.0.0.0:{}", port))
        .or_else(|err| Err(GeckoDBError::IoError(err)))?;
    loop {
        match listener.accept() {
            Ok((stream, _)) => {
                let server = GdbServer::new(stream, parser);
                match server.run()? {
                    DisconnectReason::Disconnect => continue,
                    DisconnectReason::Kill => continue,
                    DisconnectReason::TargetExited(_) => break,
                    DisconnectReason::TargetTerminated(_) => break,
                }
            }
            Err(err) => {
                debug!("Error while waiting new connection: {}", err);
                match err.kind() {
                ErrorKind::BrokenPipe | ErrorKind::ConnectionReset => {
                    println!("Connection closed; Waiting for new connection");
                    continue;
                }
                ErrorKind::Interrupted => {
                    println!("Interupted!");
                    break;
                }
                _ => break,
            }},
        }
    }
    Ok(())
}

// impl<E> From<USBGeckoError> for GdbStubError<USBGeckoError, E> {
//     fn from(err: USBGeckoError) -> Self {
//         GdbStubError::TargetError(err);
//     }
// }

#[derive(Debug)]
struct BreakAddr(u32);

enum BreakAddrError {
    ParseInt(ParseIntError),
    Range(u32),
}

impl From<ParseIntError> for BreakAddrError {
    fn from(value: ParseIntError) -> Self {
        BreakAddrError::ParseInt(value)
    }
}

impl FromStr for BreakAddr {
    type Err = BreakAddrError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut radix = 10;
        let mut digits = &s[0..s.len()];
        if digits.starts_with("0b") {
            radix = 2;
            digits = &digits[2..digits.len()];
        } else if digits.starts_with("0o") {
            radix = 8;
            digits = &digits[2..digits.len()];
        } else if digits.starts_with("0x") {
            radix = 16;
            digits = &digits[2..digits.len()];
        }

        let addr = u32::from_str_radix(digits, radix)?;

        if !((0x80000000 <= addr && 0x81800000 > addr) || (0x90000000 >= addr && 0x94000000 < addr))
        {
            return Err(BreakAddrError::Range(addr));
        }

        Ok(BreakAddr(addr))
    }
}

impl From<u32> for BreakAddr {
    fn from(value: u32) -> Self {
        BreakAddr(value)
    }
}

fn main() -> Result<(), GeckoDBError<USBGeckoError, <TcpStream as Connection>::Error>> {
    let mut verbose = LevelFilter::Warn;
    let mut device_addr: Option<SocketAddr> = None;
    let mut break_addr: Option<BreakAddr> = None;
    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Allows debugging of GameCube/Wii games through a USBGecko.");
        ap.refer(&mut verbose)
            .add_option(
                &["-v", "--verbose"],
                StoreConst(LevelFilter::Info),
                "Be verbose.",
            )
            .add_option(
                &["-q", "--quiet"],
                StoreConst(LevelFilter::Error),
                "Be quiet.",
            )
            .add_option(
                &["-d", "--debug"],
                StoreConst(LevelFilter::Debug),
                "Set logging level to Debug.",
            );
        ap.refer(&mut device_addr)
            .add_option(&["-a", "--addr"], StoreOption, "Address to reach Dolphin's USBGecko. If not provided, will fetch from serial ports on the device");
        ap.refer(&mut break_addr).add_option(
            &["-b", "--break-addr"],
            StoreOption,
            "Memory address to inject a break on when manually interrupting the game's execution.",
        );
        ap.parse_args_or_exit();
    }

    // TODO Get the address where to put the first breakpoint
    let _bp_addr: u32 = if device_addr.is_none() {
        0x8001e6b0 // Wii NTSCU 1.0
                   // 0x8034e2c8 // Wii NTSCU 1.0
    } else {
        0x80018A6C // GC NTSCU
    };

    init_logger(verbose).ok();
    // let text = std::fs::read_to_string("assets/RframeworkF.map").ok().unwrap_or_default();
    // debug!("symbols: {:?}", symbol_parser::parse_symbols(&text));
    let mut ug = USBGeckoDevice::new(break_addr.map(|x| x.0));
    info!("Connecting to USBGecko...");
    if !ug.connect(device_addr)? {
        return Err(USBGeckoError::Uninitialized.into());
    }
    info!("Connected to USBGecko");
    debug!("USBGecko: {:?}", ug);
    debug!("Console Status: {:?}", ug.status()?);
    debug!(
        "Data from code handler: {:?}",
        ug.dump(0x80001800, 0x80001820, None as Option<UpdateFn>)
            .ok()
            .expect("a vector")
    );
    // debug!("Boss flag: {:?}", ug.dump(0x8053a8e8, 0x8053a8e9, None as Option<UpdateFn>).ok().expect("a vector"));
    // ug.upload(0x8053a8e8, 0x8053a8e9, &mut Cursor::new([0x5a]), None as Option<UpdateFn>)?;
    // debug!("Boss flag: {:?}", ug.dump(0x8053a8e8, 0x8053a8e9, None as Option<UpdateFn>).ok().expect("a vector"));
    debug!(
        "Game Code: {:?}",
        std::str::from_utf8(
            &ug.dump::<fn(usize, usize, usize, usize, bool, bool)>(0x80000000, 0x80000006, None)?
        )
    );
    // ug.breakpoint_x(0x8001e6b0)?;
    // sleep(Duration::from_millis(50));
    // debug!("Console Status: {:?}", ug.status()?);
    // debug!("Registers: {:08x?}", ug.get_registers()?);

    // ug.pause()?;
    // ug.breakpoint_x(break_addr.0)?;
    // ug.resume()?;
    // loop {
    //     match ug.breakpoint_hit() {
    //         BreakpointStatus::Hit => break,
    //         BreakpointStatus::NoReply => return Err(USBGeckoError::Uninitialized.into()),
    //         BreakpointStatus::NotHit => continue,
    //     }
    // }
    let mut parser = Parser::new(ug);
    parser.interrupt()?;
    debug!("Registers: {:08x?}", parser.get_device().get_registers()?);
    let result = run_server(&mut parser);
    let _ = parser.get_device().resume();
    debug!("QUITTING");
    result
}
