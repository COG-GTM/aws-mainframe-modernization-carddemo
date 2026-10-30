package com.carddemo.transactiontype.domain.batch;

import java.util.List;

/** Outcome of one MNTTRDB2 / COBTUPDT run: job return code plus the SYSOUT DISPLAY lines. */
public record BatchResult(int returnCode, int recordsRead, int inserted, int updated, int deleted,
                          int ignored, int errors, List<String> sysout) {
}
