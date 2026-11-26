package com.carddemo.programs.comen01c.dto;

import com.carddemo.programs.comen01c.copybook.CobolText;

/**
 * Input of map COMEN1A as delivered by {@code EXEC CICS RECEIVE MAP('COMEN1A')}: the OPTIONI content
 * (2 bytes). {@code null} means the field was not transmitted (LOW-VALUES, length 0).
 */
public record MenuScreenRequest(String optionI) {

    public MenuScreenRequest {
        if (optionI != null && optionI.length() != 2) {
            throw new IllegalArgumentException("OPTIONI is PIC X(2)");
        }
    }

    public static MenuScreenRequest notTransmitted() {
        return new MenuScreenRequest(null);
    }

    /** OPTIONI exactly as placed in COMEN1AI (padded with spaces / truncated to 2 bytes). */
    public static MenuScreenRequest raw(String optionI) {
        return new MenuScreenRequest(CobolText.move(optionI, 2));
    }

    /**
     * What the operator keyed into OPTION, after BMS input mapping with {@code JUSTIFY=(RIGHT,ZERO)} from
     * app/bms/COMEN01.bms: right-justified and zero-filled on the left.
     */
    public static MenuScreenRequest keyed(String typed) {
        if (typed == null || typed.isEmpty()) {
            return notTransmitted();
        }
        String t = typed.length() > 2 ? typed.substring(0, 2) : typed;
        return new MenuScreenRequest("0".repeat(2 - t.length()) + t);
    }
}
