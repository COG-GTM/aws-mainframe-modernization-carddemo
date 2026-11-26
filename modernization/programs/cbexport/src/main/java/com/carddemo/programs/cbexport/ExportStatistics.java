package com.carddemo.programs.cbexport;

/** WS-EXPORT-STATISTICS: six PIC 9(09) counters. */
public record ExportStatistics(
        long customerRecordsExported,
        long accountRecordsExported,
        long xrefRecordsExported,
        long tranRecordsExported,
        long cardRecordsExported,
        long totalRecordsExported) {
}
