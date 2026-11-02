package com.carddemo.common.record;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.record.layout.AccountRecordLayout;
import com.carddemo.common.record.layout.SampleDataset;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

/** The ASCII extracts and the EBCDIC {@code .PS} datasets decode to the same typed records. */
class AsciiEbcdicEquivalenceTest {

    /**
     * Field-level differences between the two copies of the sample data, as committed in {@code app/data}
     * (record number is 1-based). They are differences in the source files, not decoding errors.
     */
    private static final Map<SampleDataset, List<String>> KNOWN_DIFFERENCES = Map.of(
            SampleDataset.ACCTDATA, List.of("49 ACCT-ADDR-ZIP ascii='A000000000' ebcdic='ZEROAPR   '"),
            SampleDataset.DISCGRP, List.of("34 DIS-INT-RATE ascii=0.00 ebcdic=15.00"));

    @Test
    void firstAccountDecodesIdenticallyFromAcctdataTxtAndAcctdataPs() {
        CobolRecord ascii = SampleData.ascii(SampleDataset.ACCTDATA).get(0);
        CobolRecord ebcdic = SampleData.ebcdic(SampleDataset.ACCTDATA).get(0);

        for (CobolRecord account : List.of(ascii, ebcdic)) {
            assertThat(account.getLong(AccountRecordLayout.ACCT_ID)).isEqualTo(1L);
            assertThat(account.get(AccountRecordLayout.ACCT_ACTIVE_STATUS)).isEqualTo("Y");
            assertThat(account.get(AccountRecordLayout.ACCT_CURR_BAL)).isEqualByComparingTo("194.00");
            assertThat(account.get(AccountRecordLayout.ACCT_CREDIT_LIMIT)).isEqualByComparingTo("2020.00");
            assertThat(account.get(AccountRecordLayout.ACCT_CASH_CREDIT_LIMIT)).isEqualByComparingTo("1020.00");
            assertThat(account.get(AccountRecordLayout.ACCT_OPEN_DATE)).isEqualTo("2014-11-20");
            assertThat(account.get(AccountRecordLayout.ACCT_EXPIRAION_DATE)).isEqualTo("2025-05-20");
            assertThat(account.get(AccountRecordLayout.ACCT_REISSUE_DATE)).isEqualTo("2025-05-20");
            assertThat(account.get(AccountRecordLayout.ACCT_CURR_CYC_CREDIT)).isEqualByComparingTo("0");
            assertThat(account.getTrimmed(AccountRecordLayout.ACCT_ADDR_ZIP)).isEqualTo("A000000000");
            assertThat(account.getTrimmed(AccountRecordLayout.ACCT_GROUP_ID)).isNull();
        }
        assertThat(ascii.values()).isEqualTo(ebcdic.values());
        assertThat(ascii.withCodePage(CodePage.EBCDIC).toBytes()).isEqualTo(ebcdic.toBytes());
        assertThat(ebcdic.withCodePage(CodePage.ASCII).toBytes()).isEqualTo(ascii.toBytes());
    }

    @ParameterizedTest
    @EnumSource(value = SampleDataset.class, names = {"USRSEC", "EXPORT"}, mode = EnumSource.Mode.EXCLUDE)
    void datasetDecodesIdenticallyInBothCodePages(SampleDataset dataset) {
        List<CobolRecord> ascii = SampleData.ascii(dataset);
        List<CobolRecord> ebcdic = SampleData.ebcdic(dataset);
        assertThat(ascii).hasSameSizeAs(ebcdic);

        List<String> differences = new ArrayList<>();
        for (int i = 0; i < ascii.size(); i++) {
            Map<Field, Object> a = ascii.get(i).values();
            Map<Field, Object> e = ebcdic.get(i).values();
            boolean recordDiffers = false;
            for (Field field : dataset.layout().fields()) {
                if (!sameValue(a.get(field), e.get(field))) {
                    differences.add((i + 1) + " " + field.name() + " ascii=" + show(a.get(field))
                            + " ebcdic=" + show(e.get(field)));
                    recordDiffers = true;
                }
            }
            if (!recordDiffers) {
                assertThat(ascii.get(i).withCodePage(CodePage.EBCDIC).toBytes())
                        .as("record %d converted to CP037", i + 1)
                        .isEqualTo(ebcdic.get(i).toBytes());
            }
        }
        assertThat(differences).containsExactlyElementsOf(KNOWN_DIFFERENCES.getOrDefault(dataset, List.of()));
    }

    private static boolean sameValue(Object a, Object b) {
        if (a instanceof BigDecimal x && b instanceof BigDecimal y) {
            return x.compareTo(y) == 0;
        }
        return Objects.equals(a, b);
    }

    private static String show(Object value) {
        return value instanceof String s ? "'" + s + "'" : String.valueOf(value);
    }
}
