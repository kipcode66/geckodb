use core::num::NonZeroUsize;

use gdbstub::arch::RegId;

/// 32-bit ARM core register identifier.
#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
pub enum PowerPCBroadwayRegId {
    /// General purpose registers (R0-R31)
    Gpr(u8),
    /// Program Counter (R64)
    Pc,
    /// Machine State Register (R65)
    Msr,
    /// Condition Register (R66)
    Cr,
    /// Link Register (R67)
    Lr,
    /// Status Save/Restore Register 0
    SRR0,
    /// Status Save/Restore Register 1
    SRR1,
    /// Floating point registers (F0-F31)
    Fpr(u8),
    /// Floating point status
    Fps,
    /// Current Program Status Register (cpsr)
    Cpsr,
}

/// Gecko register order:
/// "  CR",
/// " XER",
/// " CTR",
/// "DSIS",
/// " DAR",
/// "SRR0",
/// "SRR1",
/// "  r0",
/// "  r1",
/// "  r2",
/// "  r3",
/// "  r4",
/// "  r5",
/// "  r6",
/// "  r7",
/// "  r8",
/// "  r9",
/// " r10",
/// " r11",
/// " r12",
/// " r13",
/// " r14",
/// " r15",
/// " r16",
/// " r17",
/// " r18",
/// " r19",
/// " r20",
/// " r21",
/// " r22",
/// " r23",
/// " r24",
/// " r25",
/// " r26",
/// " r27",
/// " r28",
/// " r29",
/// " r30",
/// " r31",
/// "  LR",
/// "  f0",
/// "  f1",
/// "  f2",
/// "  f3",
/// "  f4",
/// "  f5",
/// "  f6",
/// "  f7",
/// "  f8",
/// "  f9",
/// " f10",
/// " f11",
/// " f12",
/// " f13",
/// " f14",
/// " f15",
/// " f16",
/// " f17",
/// " f18",
/// " f19",
/// " f20",
/// " f21",
/// " f22",
/// " f23",
/// " f24",
/// " f25",
/// " f26",
/// " f27",
/// " f28",
/// " f29",
/// " f30",
/// " f31"

impl RegId for PowerPCBroadwayRegId {
    fn from_raw_id(id: usize) -> Option<(Self, Option<NonZeroUsize>)> {
        let reg = match id {
            0..=31 => PowerPCBroadwayRegId::Gpr(id as u8),
            32..=63 => PowerPCBroadwayRegId::Fpr((id - 32) as u8),
            64 => PowerPCBroadwayRegId::Pc,
            _ => return None,
        };
        println!("from_raw_id: reg = {:?}", reg);
        Some((reg, Some(NonZeroUsize::new(4)?)))
    }
}