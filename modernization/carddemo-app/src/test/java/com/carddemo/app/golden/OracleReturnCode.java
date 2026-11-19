package com.carddemo.app.golden;

import com.carddemo.common.batch.ReturnCode;

/**
 * Maps the GnuCOBOL oracle's process exit code to the Java job's JCL condition code (D-015, D-039).
 * 0 and 4 must be identical. CBTRN02C abends with {@code CALL 'CEE3ABD'} ABCODE 999 (U999); on Linux the oracle stub
 * ends with {@code STOP RUN RETURNING 999}, which the shell sees as {@code 999 mod 256 = 231} (runbook R-06). The Java
 * job reports every abend as 16, so oracle 231 ≡ Java 16. Any other oracle code has no Java equivalent and is refused.
 */
public final class OracleReturnCode {

    /** {@code 999 mod 256}: U999 as a Linux exit status. */
    public static final int GNUCOBOL_U999 = 999 % 256;

    private OracleReturnCode() {
    }

    public static int toJava(int oracleRc) {
        return switch (oracleRc) {
            case ReturnCode.OK, ReturnCode.WARNING -> oracleRc;
            case GNUCOBOL_U999 -> ReturnCode.ABEND;
            default -> throw new IllegalArgumentException("Oracle RC " + oracleRc + " has no Java equivalent (expected 0, 4 or 231)");
        };
    }
}
