package com.carddemo.common.golden;

import com.carddemo.common.cobol.DataEncoding;
import com.carddemo.common.copybook.CopybookField;
import com.carddemo.common.copybook.RecordLayout;
import com.carddemo.common.copybook.RecordView;
import com.carddemo.common.testing.CardDemoDataset;
import com.carddemo.common.testing.GoldenMaster;
import com.carddemo.common.testing.GoldenMasterTest;
import com.carddemo.common.testing.KnownDifference;
import com.carddemo.common.testing.MainframeSources;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

/** Golden-master checks of the COBOL data-type library against the real sample datasets in app/data. */
@GoldenMasterTest
class SampleDatasetGoldenMasterTest {

    /**
     * Divergences between the binary EBCDIC datasets and their ASCII copies that exist in the repository data
     * itself (the ASCII files were not regenerated after these EBCDIC edits).
     */
    private static final Map<CardDemoDataset, KnownDifference[]> KNOWN = Map.of(
            CardDemoDataset.ACCTDATA, new KnownDifference[] {
                    new KnownDifference(49, "ACCT-ADDR-ZIP", "EBCDIC has ZEROAPR, ASCII copy still A000000000")},
            CardDemoDataset.DISCGRP, new KnownDifference[] {
                    new KnownDifference(34, "DIS-INT-RATE", "EBCDIC DEFAULT/07/0001 rate is 15.00, ASCII copy 0.00")});

    @ParameterizedTest(name = "{0}")
    @EnumSource(value = CardDemoDataset.class, names = "USRSEC", mode = EnumSource.Mode.EXCLUDE)
    void asciiTransferMatchesEbcdicDataset(CardDemoDataset dataset, MainframeSources sources, GoldenMaster golden) {
        RecordLayout layout = dataset.layout(sources);
        golden.assertMatches(dataset + " EBCDIC vs ASCII", layout,
                dataset.ebcdicRecords(sources), DataEncoding.EBCDIC,
                dataset.asciiRecords(sources), DataEncoding.ASCII,
                KNOWN.getOrDefault(dataset, new KnownDifference[0]));
    }

    @ParameterizedTest(name = "{0}")
    @EnumSource(CardDemoDataset.class)
    void everyFieldDecodesAndReEncodesByteForByte(CardDemoDataset dataset, MainframeSources sources, GoldenMaster golden) {
        RecordLayout layout = dataset.layout(sources);
        List<byte[]> original = dataset.ebcdicRecords(sources);
        List<byte[]> rewritten = new ArrayList<>();
        for (byte[] record : original) {
            RecordView source = RecordView.wrap(layout, DataEncoding.EBCDIC, record);
            byte[] copy = record.clone();
            for (CopybookField f : layout.primaryElementaryFields()) {
                for (int offset : RecordView.offsets(f)) {
                    Arrays.fill(copy, offset, offset + f.length(), (byte) 0xFF);
                }
            }
            RecordView target = RecordView.wrap(layout, DataEncoding.EBCDIC, copy);
            for (CopybookField f : layout.primaryElementaryFields()) {
                if (f.pic().isNumeric()) {
                    target.setDecimal(f, source.getDecimal(f));
                } else {
                    target.setString(f, source.getString(f));
                }
            }
            rewritten.add(copy);
        }
        assertThat(rewritten).hasSameSizeAs(original);
        for (int i = 0; i < original.size(); i++) {
            assertThat(rewritten.get(i)).as("%s record %d", dataset, i + 1).isEqualTo(original.get(i));
        }
    }

    @Test
    void decodedSampleRecordsMatchApprovedSnapshot(MainframeSources sources, GoldenMaster golden) {
        List<String> lines = new ArrayList<>();
        for (CardDemoDataset dataset : CardDemoDataset.values()) {
            RecordLayout layout = dataset.layout(sources);
            List<byte[]> records = dataset.ebcdicRecords(sources);
            lines.add("# " + dataset + " (" + dataset.copybook() + ", LRECL " + layout.length() + ", "
                    + records.size() + " records)");
            for (byte[] record : records.subList(0, Math.min(3, records.size()))) {
                RecordView view = RecordView.wrap(layout, DataEncoding.EBCDIC, record);
                List<String> values = new ArrayList<>();
                for (CopybookField f : layout.primaryElementaryFields()) {
                    values.add(f.name() + "=" + (f.pic().isNumeric()
                            ? view.getDecimal(f).toPlainString()
                            : view.getString(f).stripTrailing()));
                }
                lines.add(String.join(" | ", values));
            }
        }
        golden.assertMatchesSnapshot("decoded-sample-records", lines);
    }

    @Test
    void coreCopybookLayoutsMatchApprovedSnapshot(MainframeSources sources, GoldenMaster golden) {
        List<String> lines = new ArrayList<>();
        for (String copybook : List.of("CVACT01Y", "CVACT02Y", "CVACT03Y", "CVCUS01Y", "CVTRA01Y", "CVTRA02Y",
                "CVTRA03Y", "CVTRA04Y", "CVTRA05Y", "CVTRA06Y", "CSUSR01Y", "CIPAUSMY", "CIPAUDTY")) {
            RecordLayout layout = sources.layout(copybook);
            lines.add("# " + copybook + " " + layout);
            lines.addAll(layout.describe().lines().toList());
        }
        golden.assertMatchesSnapshot("core-copybook-layouts", lines);
    }

    @Test
    void negativeAmountsInDailyTransactionsUseOverpunch(MainframeSources sources) {
        RecordLayout layout = CardDemoDataset.DALYTRAN.layout(sources);
        long negatives = CardDemoDataset.DALYTRAN.ebcdicRecords(sources).stream()
                .map(r -> RecordView.wrap(layout, DataEncoding.EBCDIC, r).getDecimal("DALYTRAN-AMT"))
                .filter(a -> a.signum() < 0)
                .count();
        assertThat(negatives).isPositive();
        BigDecimal first = RecordView.wrap(layout, DataEncoding.EBCDIC,
                CardDemoDataset.DALYTRAN.ebcdicRecords(sources).get(0)).getDecimal("DALYTRAN-AMT");
        assertThat(first).isEqualByComparingTo("504.77");
    }
}
