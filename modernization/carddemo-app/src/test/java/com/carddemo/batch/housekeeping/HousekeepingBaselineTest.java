package com.carddemo.batch.housekeeping;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.print.PrintProgramsBaselineTest;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.RecordLayout;
import com.carddemo.transaction.TranCatBalanceRecord;
import com.carddemo.transaction.TransactionRecord;
import java.io.IOException;
import java.util.List;
import org.junit.jupiter.api.Test;

/**
 * The SORT steps of COMBTRAN and PRTCATBL on the baseline's inputs: TRANSACT.BKUP (TRANBKP) + SYSTRAN (INTCALC)
 * merge into COMBTRAN's TRANSACT.COMBINED, and the TCATBALF backup sorts and reformats into PRTCATBL's
 * 41-byte TCATBALF.REPT.
 */
class HousekeepingBaselineTest {

    static List<FixedWidthRecord> records(String job, String dd, RecordLayout layout) throws IOException {
        return PrintProgramsBaselineTest.baselineFile(job, dd).stream()
                .map(l -> FixedWidthRecord.fromLine(layout, l, RecordEncoding.ASCII)).toList();
    }

    @Test
    void combtranMergesTheBackupAndSystranByTranId() throws IOException {
        RecordLayout layout = TransactionRecord.MAPPER.layout();
        List<FixedWidthRecord> backup = records("TRANBKP", "TRANSACT.BKUP", layout);
        List<FixedWidthRecord> systran = records("INTCALC", "TRANSACT", layout);
        assertThat(backup).hasSize(262);
        assertThat(systran).hasSize(50);
        assertThat(HousekeepingJobConfiguration.combine(backup, systran)).extracting(FixedWidthRecord::text)
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("COMBTRAN", "TRANSACT.COMBINED"));
    }

    @Test
    void prtcatblSortsAndReformatsTheBackup() throws IOException {
        List<FixedWidthRecord> backup = records("PRTCATBL", "TCATBALF.BKUP", TranCatBalanceRecord.MAPPER.layout());
        List<FixedWidthRecord> report = HousekeepingJobConfiguration.categoryBalanceReport(backup,
                RecordEncoding.ASCII);
        assertThat(report).allSatisfy(r -> assertThat(r.bytes())
                .hasSize(HousekeepingJobConfiguration.TCATBALF_REPT_LRECL));
        assertThat(report).extracting(FixedWidthRecord::text)
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("PRTCATBL", "TCATBALF.REPT"));
    }

    @Test
    void editPrintsEveryDigitAndDropsTheSign() {
        assertThat(HousekeepingJobConfiguration.editTttttttttdtt("0000011648G")).isEqualTo("000001164.87");
        assertThat(HousekeepingJobConfiguration.editTttttttttdtt("0000011648P")).isEqualTo("000001164.87");
        assertThat(HousekeepingJobConfiguration.editTttttttttdtt("0000000000{")).isEqualTo("000000000.00");
        assertThat(HousekeepingJobConfiguration.editTttttttttdtt("00000001234")).isEqualTo("000000012.34");
    }

    @Test
    void zonedKeysCompareByValueThenBytes() {
        RecordLayout layout = TranCatBalanceRecord.MAPPER.layout();
        FixedWidthRecord a = FixedWidthRecord.fromLine(layout, "00000000002", RecordEncoding.ASCII);
        FixedWidthRecord b = FixedWidthRecord.fromLine(layout, "00000000010", RecordEncoding.ASCII);
        FixedWidthRecord c = FixedWidthRecord.fromLine(layout, "0000000000A", RecordEncoding.ASCII);
        assertThat(Dfsort.sort(List.of(c, b, a), Dfsort.zd(1, 11))).containsExactly(a, b, c);
        assertThat(Dfsort.summary(312, 312)).isEqualTo("ICE054I 0 RECORDS - IN: 312, OUT: 312");
    }
}
