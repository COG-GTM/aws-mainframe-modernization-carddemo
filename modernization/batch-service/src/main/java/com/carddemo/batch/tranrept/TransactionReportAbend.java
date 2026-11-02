package com.carddemo.batch.tranrept;

/**
 * {@code 9999-ABEND-PROGRAM} (CEE3ABD 999): the message CBTRN03C displays before abending. The
 * step fails, the job ends with RC 16, and no TRANREPT is written, as {@code DISP=(NEW,CATLG,DELETE)}
 * deletes the new generation when the step abends.
 */
public class TransactionReportAbend extends RuntimeException {

    public TransactionReportAbend(String message) {
        super(message);
    }
}
