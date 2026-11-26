package com.carddemo.programs.cbact01c.gateway;

import com.carddemo.programs.cbact01c.copybook.CodatecnRec;

/** {@code CALL 'COBDATFT' USING CODATECN-REC}: formats CODATECN-INP-DATE into CODATECN-0UT-DATE in place. */
public interface DateFormatter {
    void call(CodatecnRec rec);
}
