package com.carddemo.programs.paudbunl.ims;

import com.carddemo.programs.paudbunl.copybook.ImsFunctionCodes;
import com.carddemo.programs.paudbunl.copybook.PautbPcb;

/**
 * {@code CALL 'CBLTDLI' USING function, pcb, io-area, ssa}. Implementations update the PCB
 * (status, segment name/level, key feedback) and, on success, overwrite the I/O area.
 */
@FunctionalInterface
public interface DliGateway {

    void cbltdli(ImsFunctionCodes function, PautbPcb pcb, byte[] ioArea, Ssa ssa);
}
