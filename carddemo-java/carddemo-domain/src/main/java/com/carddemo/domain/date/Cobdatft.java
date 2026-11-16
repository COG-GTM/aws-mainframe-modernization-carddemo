package com.carddemo.domain.date;

/**
 * Replacement for the assembler routine {@code app/asm/COBDATFT.asm} ({@code CALL 'COBDATFT' USING
 * CODATECN-REC}), which reformats a date between {@code YYYYMMDD} and {@code YYYY-MM-DD}. Only
 * {@code CBACT01C} calls it (type {@code '2'} in, type {@code '2'} out, for the account reissue date).
 *
 * <p>The routine moves characters by position and checks nothing else, so neither does this:
 *
 * <ul>
 *   <li>Type {@code '1'}: output type {@code '2'} is an error; any other output type gives
 *       {@code in[0-3] '-' in[4-5] '-' in[6-7]} in {@code CODATECN-0UT-DATE} bytes 0-9.
 *   <li>Type {@code '2'}: output type {@code '1'} is an error; any other output type gives
 *       {@code in[0-3] in[5-6] in[8-9]} in bytes 0-7.
 *   <li>Any other type is an error. On error {@code CODATECN-ERROR-MSG} gets {@code INVALID INPUT}.
 *   <li>Bytes of the output date the routine does not write keep their previous value, and the error
 *       message is only written on error; the routine always returns with R15 = 0.
 * </ul>
 *
 * <p>Two assembler quirks, kept as observed behaviour:
 *
 * <ul>
 *   <li>The type-1 check {@code CLC COINPDT+4,=C'-'} has no explicit length, so it compares
 *       {@code L'COINPDT} = 20 bytes, not 1, against the literal pool ({@code -INVALID INPUT} and what
 *       follows the CSECT). It cannot match real data, so a dash at offset 4 is <em>not</em> rejected.
 *   <li>{@code MVC COERMSG,=C'INVALID INPUT'} likewise moves {@code L'COERMSG} = 38 bytes: the 13
 *       characters of the literal, then 25 bytes of whatever follows it in the load module. Those are
 *       not defined by the source, so this port pads with spaces; no caller reads the message.
 * </ul>
 */
public final class Cobdatft {

    /** The literal {@code COBDATFT} moves to {@code COERMSG} on error. */
    public static final String INVALID_INPUT = "INVALID INPUT";

    private Cobdatft() {
    }

    /** {@code CALL 'COBDATFT' USING CODATECN-REC}: returns the record as the routine leaves it. */
    public static Codatecn call(Codatecn rec) {
        String in = rec.inpDate();
        char[] out = rec.outDate().toCharArray();
        switch (rec.type()) {
            case Codatecn.YYYYMMDD_IN -> {
                if (rec.outType() == Codatecn.YYYYMMDD_OP) {
                    return error(rec);
                }
                in.getChars(0, 4, out, 0);
                out[4] = '-';
                in.getChars(4, 6, out, 5);
                out[7] = '-';
                in.getChars(6, 8, out, 8);
            }
            case Codatecn.YYYY_MM_DD_IN -> {
                if (rec.outType() == Codatecn.YYYY_MM_DD_OP) {
                    return error(rec);
                }
                in.getChars(0, 4, out, 0);
                in.getChars(5, 7, out, 4);
                in.getChars(8, 10, out, 6);
            }
            default -> {
                return error(rec);
            }
        }
        return new Codatecn(rec.type(), in, rec.outType(), new String(out), rec.errorMsg());
    }

    private static Codatecn error(Codatecn rec) {
        return new Codatecn(rec.type(), rec.inpDate(), rec.outType(), rec.outDate(), INVALID_INPUT);
    }
}
