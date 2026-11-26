package com.carddemo.programs.cobswait.copybook;

/**
 * {@code 01 MVSWAIT-TIME PIC 9(8) COMP.} (COBSWAIT WORKING-STORAGE).
 *
 * <p>Unsigned 8-digit binary fullword passed BY REFERENCE to the MVSWAIT assembler routine. The
 * unit is centiseconds (hundredths of a second); see app/jcl/WAITSTEP.jcl.
 */
public record MvswaitTime(long centiseconds) {

    public static final int DIGITS = 8;
    public static final long MAX = 99_999_999L;
    /** Storage size of a PIC 9(5)-9(9) COMP item (fullword). */
    public static final int BYTES = 4;

    public MvswaitTime {
        if (centiseconds < 0 || centiseconds > MAX) {
            throw new IllegalArgumentException(
                    "MVSWAIT-TIME PIC 9(8) COMP out of range: " + centiseconds);
        }
    }

    public static final MvswaitTime ZERO = new MvswaitTime(0);

    /** Duration in milliseconds (1 centisecond = 10 ms). */
    public long millis() {
        return centiseconds * 10L;
    }
}
