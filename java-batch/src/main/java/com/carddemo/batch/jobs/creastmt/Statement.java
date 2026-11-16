package com.carddemo.batch.jobs.creastmt;

import java.util.List;

/** One account statement: its STMTFILE (80-byte) and HTMLFILE (100-byte) records. */
public record Statement(List<String> stmtRecords, List<String> htmlRecords) {
}
