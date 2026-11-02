package com.carddemo.common.record;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.common.record.layout.AccountRecordLayout;
import com.carddemo.common.record.layout.ExportRecordLayout;
import com.carddemo.common.record.layout.SampleDataset;
import com.carddemo.common.record.layout.TranCatBalRecordLayout;

import org.junit.jupiter.api.Test;

class RecordLayoutTest {

    @Test
    void pictureLengthsFollowUsage() {
        assertThat(Picture.parse("X(16)").length()).isEqualTo(16);
        assertThat(Picture.parse("9(11)").length()).isEqualTo(11);
        assertThat(Picture.parse("S9(10)V99").length()).isEqualTo(12);
        assertThat(Picture.parse("S9(10)V99 COMP-3").length()).isEqualTo(7);
        assertThat(Picture.parse("S9(09)V99 COMP-3").length()).isEqualTo(6);
        assertThat(Picture.parse("9(03) COMP-3").length()).isEqualTo(2);
        assertThat(Picture.parse("9(03) COMP").length()).isEqualTo(2);
        assertThat(Picture.parse("9(9) COMP").length()).isEqualTo(4);
        assertThat(Picture.parse("9(11) COMP").length()).isEqualTo(8);
        Picture amount = Picture.parse("s9(04)v99");
        assertThat(amount.signed()).isTrue();
        assertThat(amount.digits()).isEqualTo(6);
        assertThat(amount.scale()).isEqualTo(2);
    }

    @Test
    void unsupportedPicturesAreRejected() {
        assertThatThrownBy(() -> Picture.parse("X(5)9")).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> Picture.parse("Z(5)9")).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> Picture.parse("9(19)")).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> Picture.parse("X(5) COMP-3")).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> Picture.parse("9(5) COMP-1")).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void everyLayoutMatchesItsRecordLength() {
        int[] expected = {300, 150, 50, 500, 350, 50, 50, 60, 60, 80, 500};
        SampleDataset[] datasets = {SampleDataset.ACCTDATA, SampleDataset.CARDDATA, SampleDataset.CARDXREF,
                SampleDataset.CUSTDATA, SampleDataset.DALYTRAN, SampleDataset.DISCGRP, SampleDataset.TCATBALF,
                SampleDataset.TRANCATG, SampleDataset.TRANTYPE, SampleDataset.USRSEC, SampleDataset.EXPORT};
        for (int i = 0; i < datasets.length; i++) {
            RecordLayout layout = datasets[i].layout();
            assertThat(layout.length()).as(layout.toString()).isEqualTo(expected[i]);
            int offset = 0;
            for (Field field : layout.fields()) {
                assertThat(field.offset()).as(field.toString()).isEqualTo(offset);
                offset += field.length();
            }
            assertThat(offset).isEqualTo(layout.length());
        }
    }

    @Test
    void offsetsMatchTheCopybook() {
        assertThat(AccountRecordLayout.ACCT_CURR_BAL.offset()).isEqualTo(12);
        assertThat(AccountRecordLayout.ACCT_GROUP_ID.offset()).isEqualTo(112);
        assertThat(TranCatBalRecordLayout.TRAN_CAT_KEY.offset()).isZero();
        assertThat(TranCatBalRecordLayout.TRAN_CAT_KEY.length()).isEqualTo(17);
        assertThat(TranCatBalRecordLayout.TRAN_CAT_BAL.offset()).isEqualTo(17);
        assertThat(ExportRecordLayout.EXPORT_RECORD_DATA.offset()).isEqualTo(40);
        assertThat(ExportRecordLayout.EXP_TRAN_AMT.offset()).isEqualTo(40 + 132);
        assertThat(ExportRecordLayout.LAYOUT.view(ExportRecordLayout.CARD_VIEW)).hasSize(7);
        assertThat(ExportRecordLayout.LAYOUT.field("EXP-CUST-PHONE-NUM(2)").offset()).isEqualTo(40 + 4 + 75 + 150 + 15 + 15);
    }

    @Test
    void builderChecksLengthAndRedefines() {
        RecordLayout.Builder short1 = RecordLayout.builder("TEST", "REC");
        short1.alpha("A", "X(5)");
        assertThatThrownBy(() -> short1.build(6)).hasMessageContaining("is 5 bytes, expected 6");

        RecordLayout.Builder b = RecordLayout.builder("TEST", "REC");
        AlphanumericField data = b.alpha("DATA", "X(10)");
        b.redefines("DATA-R", data).alpha("PART", "X(4)");
        assertThatThrownBy(() -> b.build(10)).hasMessageContaining("view DATA-R ends at 4");

        RecordLayout.Builder c = RecordLayout.builder("TEST", "REC");
        AlphanumericField d = c.alpha("DATA", "X(4)");
        RecordLayout.Builder view = c.redefines("DATA-R", d);
        assertThatThrownBy(() -> view.alpha("TOO-LONG", "X(5)")).hasMessageContaining("overflows");
    }
}
