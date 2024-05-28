use std::io::Cursor;

use crate::ppc_id;
use crate::symbol_parser::Symbol;
use crate::usbgecko::{BreakpointStatus, USBGeckoDevice, USBGeckoError, UpdateFn, WiiStatus};
use byteorder::{ByteOrder, BE};
use gdbstub::target::ext::base::singlethread::{SingleThreadResume, SingleThreadSingleStep};
use gdbstub::target::ext::breakpoints::{Breakpoints, HwBreakpoint, SwBreakpoint};
use gdbstub::target::ext::extended_mode::{AttachKind, ExtendedMode, ShouldTerminate};
use gdbstub::{
    arch::Arch,
    target::{
        ext::base::{singlethread::SingleThreadBase, BaseOps},
        Target, TargetError,
    },
};
use log::{debug, warn};

// pub enum ParserError {
//     InvalidQuery,
//     DataFetchError(USBGeckoError),
// }

pub struct Parser {
    gecko: USBGeckoDevice,
    symbols: Option<Symbol>,
}

impl Target for Parser {
    type Arch = gdbstub_arch::ppc::PowerPcAltivec32<ppc_id::PowerPCBroadwayRegId>;
    type Error = USBGeckoError;

    fn base_ops(&mut self) -> gdbstub::target::ext::base::BaseOps<'_, Self::Arch, Self::Error> {
        debug!("base_ops()");
        BaseOps::SingleThread(self)
    }

    fn support_breakpoints(
        &mut self,
    ) -> Option<gdbstub::target::ext::breakpoints::BreakpointsOps<'_, Self>> {
        debug!("support_breakpoints()");
        Some(self)
    }

    fn guard_rail_implicit_sw_breakpoints(&self) -> bool {
        debug!("guard_rail_implicit_sw_breakpoints()");
        true
    }

    fn support_extended_mode(
        &mut self,
    ) -> Option<gdbstub::target::ext::extended_mode::ExtendedModeOps<'_, Self>> {
        debug!("support_extended_mode()");
        Some(self)
    }
}

impl ExtendedMode for Parser {
    fn run(
        &mut self,
        filename: Option<&[u8]>,
        args: gdbstub::target::ext::extended_mode::Args<'_, '_>,
    ) -> gdbstub::target::TargetResult<gdbstub::common::Pid, Self> {
        debug!("run(): filename={:?}, args={:?}", filename, args);
        Err(TargetError::NonFatal)
    }

    fn attach(&mut self, pid: gdbstub::common::Pid) -> gdbstub::target::TargetResult<(), Self> {
        debug!("attach(): pid={:?}", pid);
        Ok(())
    }

    fn query_if_attached(
        &mut self,
        pid: gdbstub::common::Pid,
    ) -> gdbstub::target::TargetResult<gdbstub::target::ext::extended_mode::AttachKind, Self> {
        debug!("query_if_attached(): pid={:?}", pid);
        Ok(AttachKind::Attach)
    }

    fn kill(
        &mut self,
        pid: Option<gdbstub::common::Pid>,
    ) -> gdbstub::target::TargetResult<gdbstub::target::ext::extended_mode::ShouldTerminate, Self>
    {
        debug!("kill(): pid={:?}", pid);
        self.gecko.resume()?;
        Ok(ShouldTerminate::No)
    }

    fn restart(&mut self) -> Result<(), Self::Error> {
        unimplemented!("Can't restart the program")
    }
}

impl Breakpoints for Parser {
    fn support_hw_breakpoint(
        &mut self,
    ) -> Option<gdbstub::target::ext::breakpoints::HwBreakpointOps<'_, Self>> {
        debug!("support_hw_breakpoint()");
        Some(self)
    }

    fn support_sw_breakpoint(
        &mut self,
    ) -> Option<gdbstub::target::ext::breakpoints::SwBreakpointOps<'_, Self>> {
        debug!("support_sw_breakpoint()");
        // Some(self)
        None
    }
}

impl HwBreakpoint for Parser {
    fn add_hw_breakpoint(
        &mut self,
        addr: <Self::Arch as Arch>::Usize,
        kind: <Self::Arch as Arch>::BreakpointKind,
    ) -> gdbstub::target::TargetResult<bool, Self> {
        debug!(
            "add_hw_breakpoint: addr = {:08x}; bp_kind = {:?}",
            addr, kind
        );
        if kind == 4 {
            self.gecko.breakpoint_x(addr)?;
            return Ok(true);
        }
        Ok(false)
    }

    fn remove_hw_breakpoint(
        &mut self,
        addr: <Self::Arch as Arch>::Usize,
        kind: <Self::Arch as Arch>::BreakpointKind,
    ) -> gdbstub::target::TargetResult<bool, Self> {
        debug!(
            "remove_hw_breakpoint: addr = {:08x}; bp_kind = {:?}",
            addr, kind
        );
        self.gecko.cancel_breakpoint()?;
        Ok(true)
    }
}

impl SwBreakpoint for Parser {
    fn add_sw_breakpoint(
        &mut self,
        addr: <Self::Arch as Arch>::Usize,
        kind: <Self::Arch as Arch>::BreakpointKind,
    ) -> gdbstub::target::TargetResult<bool, Self> {
        debug!(
            "add_sw_breakpoint(): addr = {:08x}; bp_kind = {:?}",
            addr, kind
        );
        self.add_hw_breakpoint(addr, kind)
    }

    fn remove_sw_breakpoint(
        &mut self,
        addr: <Self::Arch as Arch>::Usize,
        kind: <Self::Arch as Arch>::BreakpointKind,
    ) -> gdbstub::target::TargetResult<bool, Self> {
        debug!(
            "remove_sw_breakpoint(): addr = {:08x}; bp_kind = {:?}",
            addr, kind
        );
        self.remove_hw_breakpoint(addr, kind)
    }
}

impl From<USBGeckoError> for TargetError<USBGeckoError> {
    fn from(err: USBGeckoError) -> Self {
        TargetError::Fatal(err)
    }
}

impl SingleThreadBase for Parser {
    fn read_registers(
        &mut self,
        regs: &mut <Self::Arch as Arch>::Registers,
    ) -> gdbstub::target::TargetResult<(), Self> {
        debug!("read_registers: registers requested");
        let registers = self.gecko.get_registers()?;
        regs.cr = registers[0];
        regs.xer = registers[1];
        regs.ctr = registers[2];
        regs.pc = registers[5];
        regs.r.copy_from_slice(&registers[7..=38]);
        regs.lr = registers[39];
        regs.f.copy_from_slice(
            &registers[40..=71]
                .iter()
                .map(|x| unsafe { std::mem::transmute::<u32, f32>(*x) } as f64)
                .collect::<Vec<_>>()[..],
        );
        debug!("read_registers: {:08x?}", registers);
        debug!("read_registers: regs = {:?}", regs);
        Ok(())
    }

    fn write_registers(
        &mut self,
        regs: &<Self::Arch as Arch>::Registers,
    ) -> gdbstub::target::TargetResult<(), Self> {
        debug!("write_registers: {:?}", regs);
        let mut registers: Vec<u32> = std::iter::repeat(0).take(72).collect();
        registers[0] = regs.cr;
        registers[1] = regs.xer;
        registers[2] = regs.ctr;
        registers[5] = regs.pc;
        registers[7..=38].copy_from_slice(&regs.r);
        registers[39] = regs.lr;
        registers[40..=71].copy_from_slice(
            &regs
                .f
                .iter()
                .map(|x| unsafe { std::mem::transmute::<f32, u32>(*x as f32) })
                .collect::<Vec<_>>()[..],
        );
        self.gecko.send_registers(&mut Cursor::new(
            registers
                .iter()
                .map(|n| {
                    let mut buf: Vec<u8> = std::iter::repeat(0u8).take(4).collect();
                    BE::write_u32(&mut buf, *n);
                    buf
                })
                .flatten()
                .collect::<Vec<u8>>(),
        ))?;
        Ok(())
    }

    fn support_single_register_access(
        &mut self,
    ) -> Option<
        gdbstub::target::ext::base::single_register_access::SingleRegisterAccessOps<'_, (), Self>,
    > {
        debug!("support_single_register_access()");
        None
    }

    fn read_addrs(
        &mut self,
        start_addr: <Self::Arch as gdbstub::arch::Arch>::Usize,
        data: &mut [u8],
    ) -> gdbstub::target::TargetResult<usize, Self> {
        debug!("read_addrs: start_addr = {:08x}", start_addr);
        if !((start_addr >= 0x80000000 && start_addr + (data.len() as u32) < 0x81800000)
            || (start_addr > 0x90000000 && start_addr + (data.len() as u32) < 0x94000000))
        {
            warn!("Invalid address requested {:08x}", start_addr);
            data.fill(0x00);
            return Ok(data.len());
        }
        let d = self.gecko.dump(
            start_addr,
            start_addr + data.len() as u32,
            None as Option<UpdateFn>,
        )?;
        if d.len() == 0 {
            warn!("no data received");
            return Ok(0);
        }
        data.copy_from_slice(&d[..data.len()]);
        Ok(data.len())
    }

    fn write_addrs(
        &mut self,
        start_addr: <Self::Arch as Arch>::Usize,
        data: &[u8],
    ) -> gdbstub::target::TargetResult<(), Self> {
        debug!("write_addrs: start_addr = {:08x}", start_addr);
        debug!("write_addrs: data = {:?}", data);
        self.gecko.upload(
            start_addr,
            start_addr + data.len() as u32,
            &mut Cursor::new(data),
            None as Option<UpdateFn>,
        )?;
        Ok(())
    }

    fn support_resume(
        &mut self,
    ) -> Option<gdbstub::target::ext::base::singlethread::SingleThreadResumeOps<'_, Self>> {
        debug!("support_resume()");
        Some(self)
    }
}

impl SingleThreadResume for Parser {
    fn resume(&mut self, _: Option<gdbstub::common::Signal>) -> Result<(), Self::Error> {
        debug!("resume()");
        self.gecko.resume()
    }

    fn support_single_step(
        &mut self,
    ) -> Option<gdbstub::target::ext::base::singlethread::SingleThreadSingleStepOps<'_, Self>> {
        debug!("support_single_step()");
        Some(self)
    }
}

impl SingleThreadSingleStep for Parser {
    fn step(&mut self, signal: Option<gdbstub::common::Signal>) -> Result<(), Self::Error> {
        debug!("step: signal = {:?}", signal);
        self.gecko.step()
    }
}

impl Parser {
    pub fn new(device: USBGeckoDevice) -> Self {
        Self {
            gecko: device,
            symbols: None,
        }
    }

    pub fn get_device<'a>(&'a mut self) -> &'a mut USBGeckoDevice {
        &mut self.gecko
    }

    pub fn interrupt(&mut self) -> Result<(), USBGeckoError> {
        debug!("Program gets interrupted");
        if let Some(bp_addr) = self.gecko.bp_addr {
            self.gecko.pause()?;
            self.gecko.breakpoint_x(bp_addr)?;
            self.gecko.resume()?;
            loop {
                match self.gecko.breakpoint_hit() {
                    BreakpointStatus::Hit => break,
                    BreakpointStatus::NoReply => return Err(USBGeckoError::Uninitialized.into()),
                    BreakpointStatus::NotHit => continue,
                }
            }
            debug!("Status: {:?}", self.gecko.status()?);
            return Ok(());
        }
        self.gecko.pause()?;
        loop {
            match self.gecko.status()? {
                WiiStatus::Running => {
                    self.gecko.pause()?;
                    continue;
                }
                WiiStatus::Loader => {
                    return Err(USBGeckoError::Uninitialized);
                }
                WiiStatus::Paused | WiiStatus::Breakpoint => break,
                WiiStatus::Unknown => return Err(USBGeckoError::Unknown(None)),
            }
        }
        debug!("Status: {:?}", self.gecko.status()?);
        Ok(())
    }
}
