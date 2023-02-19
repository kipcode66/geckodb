use byteorder::{BE, ByteOrder};
use libftd2xx::{DeviceStatus, FtStatus, Ftdi, FtdiCommon, TimeoutError, Version};
use std::{
    error::Error,
    fmt::Display,
    io::{Read, Seek, SeekFrom, Write},
    net::{TcpStream, ToSocketAddrs},
    time::Duration,
};

pub const CHUNKSIZE: usize = 0xf800;
pub const UPL_CHUNKSIZE: usize = 0xf80;
pub const READ_TIMEOUT: Duration = Duration::from_millis(1000);
pub const WRITE_TIMEOUT: Duration = Duration::from_millis(1000);
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Command {
    Poke08 = 0x01,
    Poke16 = 0x02,
    Poke32 = 0x03,
    ReadMem = 0x04,
    Pause = 0x06,
    Resume = 0x07,
    FrameAdvance = 0x08,
    Breakpoint = 0x09,
    BreakpointX = 0x10,
    SendRegs = 0x2f,
    GetRegs = 0x30,
    CancelBp = 0x38,
    SendCheats = 0x40,
    Upload = 0x41,
    Hook = 0x42,
    HookPause = 0x43,
    Step = 0x44,
    Status = 0x50,
    StatusFast = 0x51,
    CheatExec = 0x60,
    NBreakpoint = 0x89,
    Version = 0x99,
    // Meta commands
    ConsoleAck = 0xAA,
    ConsoleRetry = 0xBB,
    ConsoleFail = 0xCC,
    ConsoleDone = 0xFF,
}

impl Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Command::Poke08 => write!(f, "Command::Poke08 (0x01)"),
            Command::Poke16 => write!(f, "Command::Poke16 (0x02)"),
            Command::Poke32 => write!(f, "Command::Poke32 (0x03)"),
            Command::ReadMem => write!(f, "Command::ReadMem (0x04)"),
            Command::Pause => write!(f, "Command::Pause (0x06)"),
            Command::Resume => write!(f, "Command::Resume (0x07)"),
            Command::FrameAdvance => write!(f, "Command::FrameAdvance (0x08)"),
            Command::Breakpoint => write!(f, "Command::Breakpoint (0x09)"),
            Command::BreakpointX => write!(f, "Command::BreakpointX (0x10)"),
            Command::SendRegs => write!(f, "Command::SendRegs (0x2f)"),
            Command::GetRegs => write!(f, "Command::GetRegs (0x30)"),
            Command::CancelBp => write!(f, "Command::CancelBp (0x38)"),
            Command::SendCheats => write!(f, "Command::SendCheats (0x40)"),
            Command::Upload => write!(f, "Command::Upload (0x41)"),
            Command::Hook => write!(f, "Command::Hook (0x42)"),
            Command::HookPause => write!(f, "Command::HookPause (0x43)"),
            Command::Step => write!(f, "Command::Step (0x44)"),
            Command::Status => write!(f, "Command::Status (0x50)"),
            Command::StatusFast => write!(f, "Command::StatusFast (0x51)"),
            Command::CheatExec => write!(f, "Command::CheatExec (0x60)"),
            Command::NBreakpoint => write!(f, "Command::NBreakpoint (0x89)"),
            Command::Version => write!(f, "Command::Version (0x99)"),
            Command::ConsoleAck => write!(f, "Command::ConsoleAck (0xAA)"),
            Command::ConsoleRetry => write!(f, "Command::ConsoleRetry (0xBB)"),
            Command::ConsoleFail => write!(f, "Command::ConsoleFail (0xCC)"),
            Command::ConsoleDone => write!(f, "Command::ConsoleDone (0xFF)"),
        }
    }
}

#[derive(Debug)]
pub enum USBGeckoError {
    Uninitialized,
    FTDIQuerryError(FtStatus),
    NoFTDIDeviceFound(FtStatus),
    NoUSBGeckoFound(Box<dyn Error>),
    FTDIResetError(FtStatus),
    FTDIPurgeRxTxError(FtStatus),
    FTDITimeoutSetError(FtStatus),
    FTDITransferSetError(FtStatus),
    FTDICommandSendError(FTDICommandError, Command),
    FTDIReadDataError(Option<Box<dyn Error>>),
    FTDIInvalidReply(Option<FtStatus>),
    TooManyRetries(Box<dyn Error>, usize),
    REGStreamSizeInvalid(usize),
    CheatStreamSizeInvalid(FtStatus, usize),
    InvalidAddr(u32),
    IoError(std::io::Error),
    Unknown(Option<Box<dyn Error>>),
}

impl From<FtStatus> for USBGeckoError {
    fn from(err: FtStatus) -> Self {
        return match err {
            FtStatus::DEVICE_LIST_NOT_READY => USBGeckoError::NoFTDIDeviceFound(err),
            FtStatus::DEVICE_NOT_FOUND
            | FtStatus::DEVICE_NOT_OPENED
            | FtStatus::DEVICE_NOT_OPENED_FOR_ERASE
            | FtStatus::DEVICE_NOT_OPENED_FOR_WRITE => {
                USBGeckoError::NoUSBGeckoFound(Box::new(err))
            }
            FtStatus::EEPROM_ERASE_FAILED
            | FtStatus::EEPROM_NOT_PRESENT
            | FtStatus::EEPROM_NOT_PROGRAMMED
            | FtStatus::EEPROM_READ_FAILED
            | FtStatus::EEPROM_WRITE_FAILED
            | FtStatus::FAILED_TO_WRITE_DEVICE
            | FtStatus::INSUFFICIENT_RESOURCES
            | FtStatus::INVALID_ARGS
            | FtStatus::INVALID_BAUD_RATE
            | FtStatus::INVALID_HANDLE
            | FtStatus::INVALID_PARAMETER
            | FtStatus::IO_ERROR
            | FtStatus::NOT_SUPPORTED
            | FtStatus::OTHER_ERROR => USBGeckoError::Unknown(Some(Box::new(err))),
        };
    }
}

impl From<std::io::Error> for USBGeckoError {
    fn from(value: std::io::Error) -> Self {
        Self::IoError(value)
    }
}

impl Display for USBGeckoError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            USBGeckoError::Uninitialized => write!(f, "USBGeckoError: Device uninitialized"),
            USBGeckoError::FTDIQuerryError(err) => {
                write!(f, "USBGeckoError[FTDIQuerryError]: {}", err)
            }
            USBGeckoError::NoFTDIDeviceFound(err) => {
                write!(f, "USBGeckoError[NoFTDIDeviceFound]: {}", err)
            }
            USBGeckoError::NoUSBGeckoFound(err) => {
                write!(f, "USBGeckoError[NoUSBGeckoFound]: {}", err)
            }
            USBGeckoError::FTDIResetError(err) => {
                write!(f, "USBGeckoError[FTDIResetError]: {}", err)
            }
            USBGeckoError::FTDIPurgeRxTxError(err) => {
                write!(f, "USBGeckoError[FTDIPurgeRxTxError]: {}", err)
            }
            USBGeckoError::FTDITimeoutSetError(err) => {
                write!(f, "USBGeckoError[FTDITimeoutSetError]: {}", err)
            }
            USBGeckoError::FTDITransferSetError(err) => {
                write!(f, "USBGeckoError[FTDITransferSetError]: {}", err)
            }
            USBGeckoError::FTDICommandSendError(err, cmd) => {
                write!(
                    f,
                    "USBGeckoError[FTDICommandSendError]: {} (cmd={})",
                    err, cmd
                )
            }
            USBGeckoError::FTDIReadDataError(err) => match err {
                Some(e) => write!(f, "USBGeckoError[FTDIReadDataError]: {}", e),
                None => write!(f, "USBGeckoError[FTDIReadDataError]"),
            },
            USBGeckoError::FTDIInvalidReply(err) => match err {
                Some(e) => write!(f, "USBGeckoError[FTDIInvalidReply]: {}", e),
                None => write!(f, "USBGeckoError[FTDIInvalidReply]"),
            },
            USBGeckoError::TooManyRetries(err, num_retries) => write!(
                f,
                "USBGeckoError[TooManyRetries]: num_retries={}, {}",
                num_retries, err
            ),
            USBGeckoError::REGStreamSizeInvalid(size) => {
                write!(f, "USBGeckoError[REGStreamSizeInvalid]: size={}", size)
            }
            USBGeckoError::CheatStreamSizeInvalid(err, size) => write!(
                f,
                "USBGeckoError[CheatStreamSizeInvalid]: size={}, {}",
                size, err
            ),
            USBGeckoError::InvalidAddr(err) => write!(f, "USBGeckoError[InvalidAddr]: {}", err),
            USBGeckoError::IoError(err) => write!(f, "USBGeckoError[IoError]: {}", err),
            USBGeckoError::Unknown(err) => match err {
                Some(e) => write!(f, "USBGeckoError[Unknown]: {}", e),
                None => write!(f, "USBGeckoError[Unknown]"),
            },
        }
    }
}

impl Error for USBGeckoError {}

#[derive(Debug)]
pub enum FTDICommandError {
    ResultError(Box<dyn Error>),
    FatalError,
    NoReply(Box<dyn Error>),
}

impl std::error::Error for FTDICommandError {}

impl From<std::io::Error> for FTDICommandError {
    fn from(value: std::io::Error) -> Self {
        match value.kind() {
            std::io::ErrorKind::WouldBlock => Self::NoReply(Box::new(value)),
            std::io::ErrorKind::InvalidInput => Self::ResultError(Box::new(value)),
            std::io::ErrorKind::TimedOut => Self::NoReply(Box::new(value)),
            std::io::ErrorKind::WriteZero => Self::NoReply(Box::new(value)),
            _ => Self::FatalError,
        }
    }
}

impl From<TimeoutError> for FTDICommandError {
    fn from(err: TimeoutError) -> Self {
        match err {
            timeout @ TimeoutError::Timeout {
                actual: _,
                expected: _,
            } => FTDICommandError::NoReply(Box::new(timeout)),
            timeout @ TimeoutError::FtStatus(_status) => {
                FTDICommandError::ResultError(Box::new(timeout))
            }
        }
    }
}

impl Display for FTDICommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FTDICommandError::ResultError(err) => {
                write!(f, "FTDICommandError[ResultError]: {}", err)
            }
            FTDICommandError::FatalError => write!(f, "FTDICommandError[FatalError]"),
            FTDICommandError::NoReply(err) => write!(f, "FTDICommandError[NoReply]: {}", err),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum WiiStatus {
    Running,
    Paused,
    Breakpoint,
    Loader,
    Unknown,
}

impl From<u8> for WiiStatus {
    fn from(int: u8) -> Self {
        match int {
            0 => WiiStatus::Running,
            1 => WiiStatus::Paused,
            2 => WiiStatus::Breakpoint,
            3 => WiiStatus::Loader,
            _ => WiiStatus::Unknown,
        }
    }
}

#[derive(Debug)]
pub enum Device {
    Tcp(TcpStream),
    Gecko(Ftdi),
}

impl Read for Device {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match self {
            Self::Tcp(stream) => stream.read(buf),
            Self::Gecko(device) => device
                .read(buf)
                .or_else(|err| Err(std::io::Error::new(std::io::ErrorKind::Other, err))),
        }
    }
}

impl Write for Device {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            Self::Tcp(stream) => stream.write(&buf),
            Self::Gecko(device) => device
                .write(&buf)
                .or_else(|err| Err(std::io::Error::new(std::io::ErrorKind::Other, err))),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            Self::Tcp(stream) => stream.flush(),
            Self::Gecko(device) => {
                if let Some(DeviceStatus {
                    ammount_in_tx_queue,
                    ammount_in_rx_queue: _,
                    event_status: _,
                }) = device.status().ok()
                {
                    if ammount_in_tx_queue > 0 {
                        return Err(std::io::Error::new(
                            std::io::ErrorKind::WouldBlock,
                            "Cannot flush TX data for Gecko.",
                        ));
                    }
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
pub struct USBGeckoDevice {
    device: Option<Device>,
    connected: bool,
    paused: bool,
    cached_version: u8,
    pub bp_addr: Option<u32>,
}

pub type UpdateFn = fn(usize, usize, usize, usize, bool, bool);

pub enum BreakpointStatus {
    Hit,
    NotHit,
    NoReply,
}

impl USBGeckoDevice {
    fn fetch_device() -> Result<Ftdi, FtStatus> {
        let mut device = Ftdi::with_serial_number("GECKUSB0");
        while matches!(device, Err(FtStatus::DEVICE_LIST_NOT_READY)) {
            device = Ftdi::with_serial_number("GECKUSB0");
        }
        device
    }
    pub fn new(bp_addr: Option<u32>) -> Self {
        Self {
            device: None,
            connected: false,
            paused: false,
            cached_version: 0x7f,
            bp_addr
        }
    }

    fn get_device(&mut self) -> Result<&mut Device, USBGeckoError> {
        self.device.as_mut().ok_or(USBGeckoError::Uninitialized)
    }

    pub fn reset_device(&mut self) -> Result<(), USBGeckoError> {
        if let Device::Gecko(device) = self.get_device()? {
            return device
                .reset()
                .or_else(|status| Err(USBGeckoError::FTDIResetError(status)));
        }
        Ok(())
    }

    #[cfg(windows)]
    pub fn reset_port(&mut self) -> Result<(), USBGeckoError> {
        if let Device::Gecko(device) = self.get_device()? {
            return device
                .reset_port()
                .or_else(|status| Err(USBGeckoError::FTDIResetError(status)));
        }
        Ok(())
    }

    #[cfg(windows)]
    pub fn cycle_port(&mut self) -> Result<(), USBGeckoError> {
        if let Device::Gecko(device) = self.get_device()? {
            if device.cycle_port().is_err() {
                self.disconnect().ok();
            }
        }
        Ok(())
    }

    pub fn purge_rx_tx(&mut self) -> Result<(), USBGeckoError> {
        if let Device::Gecko(device) = self.get_device()? {
            let ret = device
                .purge_all()
                .or_else(|status| Err(USBGeckoError::FTDIPurgeRxTxError(status)));
            if ret.is_err() {
                self.disconnect().ok();
            }
            return ret;
        }
        Ok(())
    }

    pub fn get_receive_queue_count(&mut self) -> Result<usize, USBGeckoError> {
        if let Device::Gecko(device) = self.get_device()? {
            return device.queue_status().or(Ok(0));
        }
        Ok(0)
    }

    pub fn get_driver_version(&mut self) -> Result<Version, USBGeckoError> {
        if let Device::Gecko(device) = self.get_device()? {
            return device.driver_version().or(Ok(Version::new(0, 0, 0)));
        }
        Ok(Version::new(0, 0, 0))
    }

    pub fn is_driver_up_to_date(&mut self) -> Result<bool, USBGeckoError> {
        self.get_driver_version()
            .and_then(|version| Ok(version >= Version::new(2, 8, 20)))
    }

    pub fn get_leftover_bytes(&mut self) -> Result<Vec<u8>, USBGeckoError> {
        let receive_queue_count = self.get_receive_queue_count()?;
        if receive_queue_count <= 0 {
            return Ok(Vec::new());
        }
        let recieved = self.gecko_read(receive_queue_count).unwrap();
        if receive_queue_count == 1 && recieved[0] == 0x11 {
            // And exception was caught on the Console
            println!("An exception was caught on the console!");
        }
        Ok(recieved)
    }

    pub fn gecko_read(&mut self, n: usize) -> Result<Vec<u8>, FTDICommandError> {
        let mut buffer: Vec<u8> = std::iter::repeat(0).take(n).collect::<Vec<_>>();
        self.get_device()
            .or(Err(FTDICommandError::FatalError))?
            .read_exact(&mut buffer)
            .and(Ok(buffer))
            .or_else(|err| Err(err.into()))
    }

    pub fn gecko_write(&mut self, data: &[u8]) -> Result<(), FTDICommandError> {
        self.get_device()
            .or(Err(FTDICommandError::FatalError))?
            .write_all(&data)
            .or_else(|err| Err(err.into()))
    }

    pub fn send_fail(&mut self, err: Option<USBGeckoError>) -> Result<(), USBGeckoError> {
        self.raw_command(Command::ConsoleFail).ok();
        if let Some(val) = err {
            Err(val)
        } else {
            Ok(())
        }
    }

    pub fn send_fail_cancel(&mut self, err: USBGeckoError) -> Result<(), USBGeckoError> {
        self.raw_command(Command::CancelBp).ok();
        self.send_fail(Some(err))
    }

    pub fn reset_gecko(&mut self) -> Result<(), USBGeckoError> {
        self.send_fail(None)?;
        self.init_gecko()?;
        Ok(())
    }

    pub fn init_gecko(&mut self) -> Result<bool, USBGeckoError> {
        self.reset_device()?;
        self.purge_rx_tx()?;
        self.get_leftover_bytes()?;
        self.cached_version = self.version_request()?;
        if self.cached_version < 0x80 {
            self.cached_version = self.version_request()?;
        }
        Ok(self.cached_version != 0x7f)
    }

    pub fn connect<A>(&mut self, addr: Option<A>) -> Result<bool, USBGeckoError>
    where
        A: ToSocketAddrs,
    {
        if self.connected {
            self.disconnect()?;
        }
        if let Some(a) = addr {
            let stream = std::net::TcpStream::connect(a)
                .or_else(|err| Err(USBGeckoError::NoUSBGeckoFound(Box::new(err))))?;
            self.device = Some(Device::Tcp(stream));
        } else {
            match Self::fetch_device() {
                Ok(device) => self.device = Some(Device::Gecko(device)),
                Err(
                    status @ FtStatus::DEVICE_NOT_FOUND
                    | status @ FtStatus::DEVICE_LIST_NOT_READY
                    | status @ FtStatus::DEVICE_NOT_OPENED
                    | status @ FtStatus::DEVICE_NOT_OPENED_FOR_ERASE
                    | status @ FtStatus::DEVICE_NOT_OPENED_FOR_WRITE,
                ) => return Err(USBGeckoError::NoUSBGeckoFound(Box::new(status))),
                Err(status) => return Err(USBGeckoError::NoFTDIDeviceFound(status)),
            };
        }
        if let Some(Device::Gecko(device)) = &mut self.device {
            device
                .set_timeouts(READ_TIMEOUT, WRITE_TIMEOUT)
                .or_else(|err| Err(USBGeckoError::FTDITimeoutSetError(err)))?;
            device
                .set_latency_timer(Duration::from_millis(16))
                .or_else(|err| Err(USBGeckoError::FTDITimeoutSetError(err)))?;
            device
                .set_usb_parameters(0x10000)
                .or_else(|err| Err(USBGeckoError::FTDITimeoutSetError(err)))?;
        }
        if !self.init_gecko()? {
            return Ok(false);
        }
        self.connected = true;
        Ok(true)
    }

    pub fn disconnect(&mut self) -> Result<(), USBGeckoError> {
        if self.device.is_some()
            && (self.paused || self.status().unwrap_or(WiiStatus::Running) != WiiStatus::Running)
        {
            self.raw_command(Command::Resume).ok();
            self.paused = false;
        }
        if let Some(Device::Gecko(device)) = &mut self.device {
            device.close()?;
        }
        self.connected = false;
        self.device = None;
        Ok(())
    }

    pub fn connected(&self) -> bool {
        self.connected
    }

    pub fn raw_command(&mut self, id: Command) -> Result<(), FTDICommandError> {
        self.gecko_write(&[id as u8])
    }

    pub fn status(&mut self) -> Result<WiiStatus, USBGeckoError> {
        self.get_leftover_bytes()?;
        self.raw_command(Command::Status).or_else(|err| {
            self.send_fail(Some(USBGeckoError::FTDICommandSendError(
                err,
                Command::Status,
            )))
        })?;
        let mut flag = false;
        let mut retry_count: u8 = 0;
        let mut rec: Option<Vec<u8>> = None;
        loop {
            match self.gecko_read(1) {
                Ok(data) => {
                    if !flag && data[0] == 0x11 {
                        flag = true;
                        rec = Some(data);
                        continue;
                    }
                    rec = Some(data);
                }
                Err(FTDICommandError::NoReply(_)) => {
                    self.send_fail(None)?;
                    self.raw_command(Command::Status).or_else(|err| {
                        self.send_fail(Some(USBGeckoError::FTDICommandSendError(
                            err,
                            Command::Status,
                        )))
                    })?;
                    retry_count += 1;
                    if retry_count > 2 {
                        return Ok(WiiStatus::Unknown);
                    }
                    continue;
                }
                _ => {
                    self.send_fail(Some(USBGeckoError::FTDIReadDataError(None)))?;
                    break;
                }
            }
            if let Some(data) = rec.as_ref() {
                if data[0] != 0x11 {
                    break;
                }
            }
        }
        if flag {
            return Ok(WiiStatus::Breakpoint);
        }
        if let Some(data) = rec.as_mut() {
            if data[0] > 3 {
                loop {
                    match self.gecko_read(1) {
                        Ok(d) => {
                            data[0] = d[0];
                            continue;
                        }
                        _ => break,
                    }
                }
            }
            return Ok(WiiStatus::from(data[0]));
        }
        Ok(WiiStatus::Unknown)
    }

    pub fn pause(&mut self) -> Result<(), USBGeckoError> {
        let mut wii_status: WiiStatus = WiiStatus::Unknown;
        loop {
            match wii_status {
                WiiStatus::Running => break,
                WiiStatus::Unknown => wii_status = self.status()?,
                _ => return Ok(()),
            }
        }
        self.raw_command(Command::Pause).or_else(|err| {
            self.send_fail(Some(USBGeckoError::FTDICommandSendError(
                err,
                Command::Pause,
            )))
        })
    }

    pub fn resume(&mut self) -> Result<(), USBGeckoError> {
        self.raw_command(Command::Resume).or_else(|err| {
            self.send_fail(Some(USBGeckoError::FTDICommandSendError(
                err,
                Command::Resume,
            )))
        })
    }

    pub fn dump<F>(
        &mut self,
        start: u32,
        end: u32,
        mut update: Option<F>,
    ) -> Result<Vec<u8>, USBGeckoError>
    where
        F: FnMut(usize, usize, usize, usize, bool, bool) -> (),
    {
        let dumpsize = (end - start) as usize;
        let n_sector = dumpsize / CHUNKSIZE;
        let rem = dumpsize % CHUNKSIZE;
        let n_sector_to_fetch = n_sector + if rem > 0 { 1 } else { 0 };
        self.get_leftover_bytes()?;
        self.raw_command(Command::ReadMem).or_else(|err| {
            self.send_fail(Some(USBGeckoError::FTDICommandSendError(
                err,
                Command::ReadMem,
            )))
        })?;
        for attempts in 0..10 {
            match self.gecko_read(1) {
                Ok(data) => {
                    if data[0] == 0xAA {
                        break;
                    }
                }
                Err(FTDICommandError::NoReply(_)) => {
                    if attempts == 3 || attempts == 6 {
                        self.send_fail(None).ok();
                        self.raw_command(Command::ReadMem).or_else(|err| {
                            self.send_fail(Some(USBGeckoError::FTDICommandSendError(
                                err,
                                Command::ReadMem,
                            )))
                        })?;
                        continue;
                    }
                }
                Err(FTDICommandError::ResultError(err)) => {
                    self.send_fail(Some(USBGeckoError::FTDIReadDataError(Some(err))))?
                }
                Err(FTDICommandError::FatalError) => {
                    self.send_fail(Some(USBGeckoError::FTDIInvalidReply(None)))?
                }
            }
        }
        let mut addr_buf = std::iter::repeat::<u8>(0).take(8).collect::<Vec<_>>();
        BE::write_u32(&mut addr_buf[0..4], start);
        BE::write_u32(&mut addr_buf[4..8], end);
        self.gecko_write(&addr_buf).or_else(|err| {
            self.send_fail(Some(USBGeckoError::FTDICommandSendError(
                err,
                Command::ReadMem,
            )))
        })?;
        let mut buffer: Vec<u8> = Vec::new();
        let mut error_count: usize = 0;
        for current_chunk in 0..n_sector_to_fetch {
            let current_chunk_rem = if current_chunk < n_sector {
                CHUNKSIZE
            } else {
                rem
            };
            if let Some(callback) = update.as_mut() {
                callback(
                    current_chunk,
                    n_sector_to_fetch,
                    current_chunk * CHUNKSIZE,
                    dumpsize,
                    error_count == 0,
                    true,
                );
            }
            match self.gecko_read(current_chunk_rem) {
                Ok(mut data) => {
                    buffer.append(&mut data);
                }
                Err(FTDICommandError::NoReply(err) | FTDICommandError::ResultError(err)) => {
                    error_count += 1;
                    if error_count >= 9 {
                        self.send_fail(Some(USBGeckoError::TooManyRetries(err, error_count)))?;
                    }
                    if error_count % 3 != 0 {
                        self.raw_command(Command::ConsoleRetry).ok();
                    }
                    continue;
                }
                _ => self.send_fail(Some(USBGeckoError::FTDIReadDataError(None)))?,
            }
            error_count = 0;
            self.raw_command(Command::ConsoleAck).ok();
        }
        if let Some(callback) = update.as_mut() {
            callback(
                n_sector_to_fetch,
                n_sector_to_fetch,
                dumpsize,
                dumpsize,
                true,
                true,
            );
        }
        self.get_leftover_bytes().ok();
        Ok(buffer)
    }

    pub fn upload<R: Read + Seek, F>(
        &mut self,
        start: u32,
        end: u32,
        data: &mut R,
        mut update: Option<F>,
    ) -> Result<(), USBGeckoError>
    where
        F: FnMut(usize, usize, usize, usize, bool, bool) -> (),
    {
        let dumpsize = (end - start) as usize;
        let n_sector = dumpsize / UPL_CHUNKSIZE;
        let rem = dumpsize % UPL_CHUNKSIZE;
        let n_sector_to_send = n_sector + if rem > 0 { 1 } else { 0 };
        self.get_leftover_bytes()?;
        self.raw_command(Command::Upload).or_else(|err| {
            self.send_fail(Some(USBGeckoError::FTDICommandSendError(
                err,
                Command::ReadMem,
            )))
        })?;
        for attempts in 0..10 {
            match self.gecko_read(1) {
                Ok(data) => {
                    if data[0] == 0xAA {
                        break;
                    } else if attempts == 9 {
                        return Err(USBGeckoError::FTDIInvalidReply(None));
                    }
                }
                Err(FTDICommandError::NoReply(_)) => {
                    if attempts == 3 || attempts == 6 {
                        self.send_fail(None).ok();
                        self.raw_command(Command::ReadMem).or_else(|err| {
                            self.send_fail(Some(USBGeckoError::FTDICommandSendError(
                                err,
                                Command::ReadMem,
                            )))
                        })?;
                        continue;
                    }
                }
                Err(FTDICommandError::ResultError(err)) => {
                    self.send_fail(Some(USBGeckoError::FTDIReadDataError(Some(err))))?
                }
                Err(FTDICommandError::FatalError) => {
                    self.send_fail(Some(USBGeckoError::FTDIInvalidReply(None)))?
                }
            }
        }
        let mut addr_buf = std::iter::repeat::<u8>(0).take(8).collect::<Vec<_>>();
        BE::write_u32(&mut addr_buf[0..4], start);
        BE::write_u32(&mut addr_buf[4..8], dumpsize as u32);
        self.gecko_write(&addr_buf).or_else(|err| {
            self.send_fail(Some(USBGeckoError::FTDICommandSendError(
                err,
                Command::ReadMem,
            )))
        })?;
        data.seek(std::io::SeekFrom::Start(0))
            .or(Err(USBGeckoError::FTDIReadDataError(None)))?;
        let mut error_count: usize = 0;
        for current_chunk in 0..n_sector_to_send {
            let current_chunk_rem = if current_chunk < n_sector {
                UPL_CHUNKSIZE
            } else {
                rem
            };
            if let Some(callback) = update.as_mut() {
                callback(
                    current_chunk,
                    n_sector_to_send,
                    current_chunk * UPL_CHUNKSIZE,
                    dumpsize,
                    error_count == 0,
                    false,
                );
            }
            let mut buf = std::iter::repeat(0)
                .take(current_chunk_rem)
                .collect::<Vec<u8>>();
            data.read_exact(&mut buf)
                .or(Err(USBGeckoError::FTDIReadDataError(None)))?;
            match self.gecko_write(&buf) {
                Err(FTDICommandError::ResultError(err)) => {
                    error_count += 1;
                    if error_count >= 3 {
                        self.raw_command(Command::ConsoleFail).ok();
                        return Err(USBGeckoError::TooManyRetries(err, error_count));
                    }
                    data.seek(std::io::SeekFrom::Current(-1 * (current_chunk_rem as i64)))
                        .ok();
                    self.raw_command(Command::ConsoleRetry).ok();
                    continue;
                }
                Err(FTDICommandError::FatalError) => {
                    self.raw_command(Command::ConsoleFail).ok();
                    return Err(USBGeckoError::FTDIReadDataError(None));
                }
                _ => {
                    error_count = 0;
                    continue;
                }
            }
        }
        if let Some(callback) = update.as_mut() {
            callback(
                n_sector_to_send,
                n_sector_to_send,
                dumpsize,
                dumpsize,
                true,
                false,
            );
        }
        Ok(())
    }

    pub fn version_request(&mut self) -> Result<u8, USBGeckoError> {
        self.raw_command(Command::Version).or_else(|err| {
            self.send_fail(Some(USBGeckoError::FTDICommandSendError(
                err,
                Command::Version,
            )))
        })?;
        for _ in 0..3 {
            match self.gecko_read(1) {
                Ok(data) => {
                    if data.len() > 0 {
                        return Ok(data[0]);
                    }
                }
                Err(FTDICommandError::NoReply(_)) => {
                    self.raw_command(Command::Version).or_else(|err| {
                        self.send_fail(Some(USBGeckoError::FTDICommandSendError(
                            err,
                            Command::Version,
                        )))
                    })?
                }
                _ => continue,
            }
        }
        Ok(0x7f)
    }

    pub fn get_registers(&mut self) -> Result<Vec<u32>, USBGeckoError> {
        let length: usize = if self.cached_version < 0x80 {
            0xA0
        } else {
            0x120
        };
        let mut registers: Vec<u8> = std::iter::repeat(0).take(length).collect();
        for _ in 0..3 {
            self.get_leftover_bytes().ok();
            self.raw_command(Command::GetRegs).or_else(|err| {
                self.send_fail(Some(USBGeckoError::FTDICommandSendError(
                    err,
                    Command::GetRegs,
                )))
            })?;
            match self.gecko_read(length) {
                Ok(mut read_data) => {
                    match self.get_leftover_bytes() {
                        Ok(data) => {
                            if data.len() > 0 && read_data[0] == 0x11 {
                                read_data.copy_within(1.., 0);
                                read_data[length - 1] = data[0];
                            }
                        }
                        _ => {}
                    }
                    registers.copy_from_slice(&read_data);
                    break;
                }
                Err(FTDICommandError::NoReply(_)) => continue,
                _ => self.send_fail(None)?,
            }
        }
        Ok(registers
            .chunks(4)
            .map(|arr| BE::read_u32(arr))
            .collect())
    }

    pub fn send_registers<R: Read + Seek>(&mut self, regs: &mut R) -> Result<(), USBGeckoError> {
        let len = {
            let pos = regs.stream_position()?;
            let len = regs.seek(SeekFrom::End(0))?;
            if pos != len {
                regs.seek(SeekFrom::Start(pos))?;
            }
            len
        };
        let length: usize = if self.cached_version < 0x80 { 0xa0 } else { 0x120 };
        if len != length as u64 {
            return Err(USBGeckoError::REGStreamSizeInvalid(len as usize));
        }
        regs.seek(SeekFrom::Start(0))?;
        let mut buf = std::iter::repeat(0).take(length).collect::<Vec<u8>>();
        regs.read_exact(&mut buf)?;
        self.raw_command(Command::SendRegs)
            .or_else(|err| Err(USBGeckoError::FTDICommandSendError(err, Command::SendRegs)))?;
        let mut error_count: u8 = 0;
        loop {
            let data = self.gecko_read(1).or_else(|err| {
                Err(self
                    .send_fail(Some(USBGeckoError::FTDIReadDataError(Some(Box::new(err)))))
                    .expect_err("'sendfail' should always return and Err"))
            })?;
            if data[0] != Command::ConsoleAck as u8 {
                error_count += 1;
            } else {
                break;
            }
            if error_count >= 3 {
                return self.send_fail(Some(USBGeckoError::FTDIReadDataError(None)));
            }
        }
        let mut error_count: u8 = 0;
        loop {
            match self.gecko_write(&buf) {
                Err(err @ FTDICommandError::ResultError(_)) => {
                    error_count += 1;
                    if error_count >= 3 {
                        self.send_fail(Some(USBGeckoError::FTDICommandSendError(
                            err,
                            Command::SendRegs,
                        )))?
                    }
                    self.raw_command(Command::ConsoleRetry).ok();
                    continue;
                }
                Err(err @ FTDICommandError::FatalError) => self.send_fail(Some(
                    USBGeckoError::FTDICommandSendError(err, Command::SendRegs),
                ))?,
                _ => return Ok(()),
            }
        }
    }

    pub fn step(&mut self) -> Result<(), USBGeckoError> {
        self.raw_command(Command::Step).or_else(|err| {
            self.send_fail(Some(USBGeckoError::FTDICommandSendError(
                err,
                Command::Step,
            )))
        })
    }

    pub fn breakpoint(&mut self, addr: u32, bp_type: u8, exact: bool) -> Result<(), USBGeckoError> {
        let input = addr & 0xfffffff8 | bp_type as u32;
        if self.cached_version < 0x80 || !exact {
            self.raw_command(Command::Breakpoint).or_else(|err| {
                self.send_fail_cancel(USBGeckoError::FTDICommandSendError(
                    err,
                    Command::Breakpoint,
                ))
            })?;
            let mut input_buf = std::iter::repeat::<u8>(0).take(4).collect::<Vec<_>>();
            BE::write_u32(&mut input_buf[0..4], input);
            self.gecko_write(&input_buf).or_else(|err| {
                self.send_fail_cancel(USBGeckoError::FTDICommandSendError(
                    err,
                    Command::Breakpoint,
                ))
            })?;
        } else {
            self.raw_command(Command::NBreakpoint).or_else(|err| {
                self.send_fail_cancel(USBGeckoError::FTDICommandSendError(
                    err,
                    Command::NBreakpoint,
                ))
            })?;
            let mut input_buf = std::iter::repeat::<u8>(0).take(4).collect::<Vec<_>>();
            BE::write_u32(&mut input_buf[0..4], input);
            self.gecko_write(&input_buf).or_else(|err| {
                self.send_fail_cancel(USBGeckoError::FTDICommandSendError(
                    err,
                    Command::NBreakpoint,
                ))
            })?;
        }
        Ok(())
    }

    pub fn breakpoint_r(&mut self, addr: u32, exact: Option<bool>) -> Result<(), USBGeckoError> {
        self.breakpoint(addr, 5, exact.unwrap_or(true))
    }

    pub fn breakpoint_w(&mut self, addr: u32, exact: Option<bool>) -> Result<(), USBGeckoError> {
        self.breakpoint(addr, 6, exact.unwrap_or(true))
    }

    pub fn breakpoint_rw(&mut self, addr: u32, exact: Option<bool>) -> Result<(), USBGeckoError> {
        self.breakpoint(addr, 7, exact.unwrap_or(true))
    }

    pub fn breakpoint_x(&mut self, addr: u32) -> Result<(), USBGeckoError> {
        let input = addr & 0xfffffffc | 3;
        let mut buf = std::iter::repeat(0).take(0x10).collect::<Vec<u8>>();
        buf[0] = Command::BreakpointX as u8;
        BE::write_u32(&mut buf[1..=4], input);
        buf[5] = Command::BreakpointX as u8;
        BE::write_u32(&mut buf[6..=10], input);
        self.gecko_write(&buf).or_else(|err| {
            self.send_fail_cancel(USBGeckoError::FTDICommandSendError(
                err,
                Command::BreakpointX,
            ))
        })
    }

    pub fn breakpoint_hit(&mut self) -> BreakpointStatus {
        if let Ok(data) = self.gecko_read(1) {
            if data[0] == 0x11 {
                return BreakpointStatus::Hit;
            }
            if let Ok(d) = self.gecko_read(1) {
                if d[0] == 0x11 {
                    return BreakpointStatus::Hit;
                }
                return BreakpointStatus::NotHit;
            } else {
                return BreakpointStatus::NotHit;
            }
        } else {
            return BreakpointStatus::NoReply;
        }
    }

    pub fn cancel_breakpoint(&mut self) -> Result<(), USBGeckoError> {
        self.raw_command(Command::CancelBp).or_else(|err| {
            self.send_fail(Some(USBGeckoError::FTDICommandSendError(
                err,
                Command::CancelBp,
            )))
        })
    }
}

impl Drop for USBGeckoDevice {
    fn drop(&mut self) {
        self.disconnect().ok();
    }
}
