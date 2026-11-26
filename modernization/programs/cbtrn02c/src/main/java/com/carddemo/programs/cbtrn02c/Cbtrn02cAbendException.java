package com.carddemo.programs.cbtrn02c;

/** 9999-ABEND-PROGRAM: CALL 'CEE3ABD' USING ABCODE (999), TIMING (0). */
public class Cbtrn02cAbendException extends RuntimeException {

    private final int abendCode;
    private final String fileStatus;

    public Cbtrn02cAbendException(int abendCode, String reason, String fileStatus) {
        super(reason + " (ABCODE " + abendCode + ", FILE STATUS " + fileStatus + ")");
        this.abendCode = abendCode;
        this.fileStatus = fileStatus;
    }

    public int abendCode() {
        return abendCode;
    }

    public String fileStatus() {
        return fileStatus;
    }
}
