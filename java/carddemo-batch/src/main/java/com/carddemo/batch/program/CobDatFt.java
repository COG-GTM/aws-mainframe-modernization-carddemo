package com.carddemo.batch.program;

import com.carddemo.batch.record.CodatecnRec;

/**
 * Java port of the assembler date routine {@code app/asm/COBDATFT.asm} ({@code CALL 'COBDATFT' USING
 * CODATECN-REC}). It reformats by character position only - no parsing or validation of the date:
 * <ul>
 *   <li>type {@code '2'} ({@code YYYY-MM-DD} in) with out-type other than {@code '1'}: copies positions
 *       1-4, 6-7 and 9-10 of the input to positions 1-8 of the output ({@code YYYYMMDD}); bytes 9-20 of
 *       {@code CODATECN-0UT-DATE} are left untouched;</li>
 *   <li>type {@code '2'} with out-type {@code '1'}: {@code INVALID INPUT};</li>
 *   <li>type {@code '1'} ({@code YYYYMMDD} in) with out-type other than {@code '2'} and no hyphen at
 *       position 5: inserts hyphens ({@code YYYY-MM-DD}, positions 1-10);</li>
 *   <li>anything else: moves {@code INVALID INPUT} to {@code CODATECN-ERROR-MSG} and leaves the output
 *       date untouched.</li>
 * </ul>
 */
public final class CobDatFt {

    private CobDatFt() {
    }

    public static void call(CodatecnRec rec) {
        String in = rec.codatecnInpDate();
        String type = rec.codatecnType();
        String outType = rec.codatecnOuttype();
        if (CodatecnRec.YYYYMMDD.equals(type)) {
            if (in.charAt(4) == '-' || CodatecnRec.YYYY_MM_DD.equals(outType)) {
                gotoErr(rec);
            } else {
                rec.setCodatecnOutDate(1, in.substring(0, 4));
                rec.setCodatecnOutDate(5, "-");
                rec.setCodatecnOutDate(6, in.substring(4, 6));
                rec.setCodatecnOutDate(8, "-");
                rec.setCodatecnOutDate(9, in.substring(6, 8));
            }
        } else if (CodatecnRec.YYYY_MM_DD.equals(type)) {
            if (CodatecnRec.YYYYMMDD.equals(outType)) {
                gotoErr(rec);
            } else {
                rec.setCodatecnOutDate(1, in.substring(0, 4));
                rec.setCodatecnOutDate(5, in.substring(5, 7));
                rec.setCodatecnOutDate(7, in.substring(8, 10));
            }
        } else {
            gotoErr(rec);
        }
    }

    private static void gotoErr(CodatecnRec rec) {
        rec.setCodatecnErrorMsg(CodatecnRec.INVALID_INPUT);
    }
}
