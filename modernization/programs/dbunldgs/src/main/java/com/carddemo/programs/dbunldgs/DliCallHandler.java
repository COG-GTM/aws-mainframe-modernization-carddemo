package com.carddemo.programs.dbunldgs;

import com.carddemo.programs.dbunldgs.copybook.PcbMask;

/**
 * Gateway for one PCB of the PSB: {@code CALL 'CBLTDLI' USING function pcb io-area [ssa]}.
 * The implementation fills/consumes the I/O area and sets the PCB status, segment level/name and
 * key feedback exactly as IMS would.
 */
@FunctionalInterface
public interface DliCallHandler {
    void cbltdli(String function, PcbMask pcb, byte[] ioArea, String ssa);
}
