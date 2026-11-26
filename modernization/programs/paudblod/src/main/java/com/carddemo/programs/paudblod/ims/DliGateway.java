package com.carddemo.programs.paudblod.ims;

import com.carddemo.programs.paudblod.copybook.ImsFuncs;
import com.carddemo.programs.paudblod.copybook.PautbPcb;

/**
 * {@code CALL 'CBLTDLI' USING function pcb io-area ssa}. Implementations must set
 * PAUT-PCB-STATUS (and the other PCB feedback fields) and, for get calls, fill {@code ioArea}.
 */
public interface DliGateway {

    void cbltdli(ImsFuncs function, PautbPcb pcb, byte[] ioArea, Ssa ssa);
}
