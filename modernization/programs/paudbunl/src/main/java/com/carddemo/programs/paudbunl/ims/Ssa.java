package com.carddemo.programs.paudbunl.ims;

/**
 * Unqualified segment search argument: 8-byte segment name followed by a blank,
 * as in ROOT-UNQUAL-SSA ('PAUTSUM0 ') and CHILD-UNQUAL-SSA ('PAUTDTL1 ').
 */
public record Ssa(String segmentName) {

    /** 01 ROOT-UNQUAL-SSA. */
    public static final Ssa ROOT_UNQUAL_SSA = new Ssa("PAUTSUM0");
    /** 01 CHILD-UNQUAL-SSA. */
    public static final Ssa CHILD_UNQUAL_SSA = new Ssa("PAUTDTL1");

    public Ssa {
        if (segmentName.length() > 8) {
            throw new IllegalArgumentException("Segment name longer than 8: " + segmentName);
        }
    }

    /** The 9-byte SSA text passed to CBLTDLI. */
    public String text() {
        return String.format("%-8s ", segmentName);
    }
}
