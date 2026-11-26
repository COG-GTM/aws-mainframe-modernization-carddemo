package com.carddemo.programs.cbact01c.gateway;

import com.carddemo.programs.cbact01c.copybook.CodatecnRec;

/**
 * Port of assembler routine app/asm/COBDATFT.asm.
 * <ul>
 *   <li>type '1' (YYYYMMDD) to outtype '1': YYYY-MM-DD; rejected if input byte 5 is '-' or outtype is '2'.</li>
 *   <li>type '2' (YYYY-MM-DD) to outtype '2': YYYYMMDD; rejected if outtype is '1'. Separators are not checked.</li>
 *   <li>any other type: rejected.</li>
 * </ul>
 * Only the bytes listed are written; the rest of CODATECN-0UT-DATE is left as it was. A rejection moves
 * 'INVALID INPUT' to CODATECN-ERROR-MSG and leaves the output date untouched; a success does not clear an
 * earlier error message.
 */
public final class CobdatftDateFormatter implements DateFormatter {
    public static final String INVALID_INPUT = "INVALID INPUT";

    @Override
    public void call(CodatecnRec rec) {
        String in = rec.codatecnInpDate();
        StringBuilder out = new StringBuilder(rec.codatecn0utDate());
        if (rec.isYyyymmddIn()) {
            // VALIDIN1
            if (in.charAt(4) == '-' || rec.codatecnOuttype().equals("2")) {
                rec.setCodatecnErrorMsg(INVALID_INPUT);
                return;
            }
            out.replace(0, 4, in.substring(0, 4));
            out.setCharAt(4, '-');
            out.replace(5, 7, in.substring(4, 6));
            out.setCharAt(7, '-');
            out.replace(8, 10, in.substring(6, 8));
        } else if (rec.isYyyyMmDdIn()) {
            // VALIDIN2
            if (rec.codatecnOuttype().equals("1")) {
                rec.setCodatecnErrorMsg(INVALID_INPUT);
                return;
            }
            out.replace(0, 4, in.substring(0, 4));
            out.replace(4, 6, in.substring(5, 7));
            out.replace(6, 8, in.substring(8, 10));
        } else {
            // GOTOERR
            rec.setCodatecnErrorMsg(INVALID_INPUT);
            return;
        }
        rec.setCodatecn0utDate(out.toString());
    }
}
