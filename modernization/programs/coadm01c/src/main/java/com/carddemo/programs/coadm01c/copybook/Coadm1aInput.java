package com.carddemo.programs.coadm01c.copybook;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * COADM01 (cpy-bms) - 01 COADM1AI, the symbolic input map for COADM1A as filled by {@code EXEC CICS RECEIVE MAP}.
 * A field the terminal did not transmit is LOW-VALUES ({@code null} on construction).
 */
public record Coadm1aInput(String trnname, String title01, String curdate, String pgmname, String title02,
                           String curtime, List<String> optn, String option, String errmsg) {

    public static final int OPTN_COUNT = 12;
    public static final int OPTION_LENGTH = 2;

    public Coadm1aInput {
        trnname = field(trnname, Coadm1aOutput.TRNNAME_LENGTH);
        title01 = field(title01, Coadm1aOutput.TITLE_LENGTH);
        curdate = field(curdate, Coadm1aOutput.CURDATE_LENGTH);
        pgmname = field(pgmname, Coadm1aOutput.PGMNAME_LENGTH);
        title02 = field(title02, Coadm1aOutput.TITLE_LENGTH);
        curtime = field(curtime, Coadm1aOutput.CURTIME_LENGTH);
        List<String> o = new ArrayList<>(Collections.nCopies(OPTN_COUNT, (String) null));
        if (optn != null) {
            for (int i = 0; i < Math.min(optn.size(), OPTN_COUNT); i++) {
                o.set(i, optn.get(i));
            }
        }
        o.replaceAll(v -> field(v, Coadm1aOutput.OPTN_LENGTH));
        optn = List.copyOf(o);
        option = field(option, OPTION_LENGTH);
        errmsg = field(errmsg, Coadm1aOutput.ERRMSG_LENGTH);
    }

    /** Map where only OPTIONI carries data, exactly as mapped by BMS (2 bytes, or {@code null} = LOW-VALUES). */
    public static Coadm1aInput ofOption(String optionI) {
        return new Coadm1aInput(null, null, null, null, null, null, null, optionI, null);
    }

    /**
     * Map built from what the operator keyed into OPTION, applying the BMS field definition
     * {@code JUSTIFY=(RIGHT,ZERO)}: data right-justified, left-filled with zeros. Keying nothing is LOW-VALUES.
     */
    public static Coadm1aInput keyed(String keyedOption) {
        if (keyedOption == null || keyedOption.isEmpty()) {
            return ofOption(null);
        }
        if (keyedOption.length() > OPTION_LENGTH) {
            throw new IllegalArgumentException("OPTION is LENGTH=2: [" + keyedOption + "]");
        }
        return ofOption("0".repeat(OPTION_LENGTH - keyedOption.length()) + keyedOption);
    }

    private static String field(String value, int len) {
        return value == null ? CobolFields.lowValues(len) : CobolFields.alnum(value, len);
    }
}
