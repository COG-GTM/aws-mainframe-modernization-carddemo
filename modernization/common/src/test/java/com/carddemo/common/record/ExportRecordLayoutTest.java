package com.carddemo.common.record;

import static com.carddemo.common.record.layout.ExportRecordLayout.*;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.common.record.layout.ExportRecordLayout;
import com.carddemo.common.record.layout.SampleDataset;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;

/** {@code CVEXPORT} records from {@code AWS.M2.CARDDEMO.EXPORT.DATA.PS}: COMP, COMP-3 and REDEFINES. */
class ExportRecordLayoutTest {

    private final List<CobolRecord> records = SampleData.ebcdic(SampleDataset.EXPORT);

    @Test
    void recordTypesFollowCbexportOrder() {
        Map<String, Long> counts = records.stream()
                .collect(Collectors.groupingBy(r -> r.get(EXPORT_REC_TYPE), Collectors.counting()));
        assertThat(counts).containsExactlyInAnyOrderEntriesOf(Map.of("C", 50L, "A", 50L, "X", 50L, "T", 300L, "D", 50L));
        assertThat(records.get(0).getLong(EXPORT_SEQUENCE_NUM)).isEqualTo(1L);
        for (int i = 1; i < records.size(); i++) {
            assertThat(records.get(i).getLong(EXPORT_SEQUENCE_NUM)).isGreaterThan(records.get(i - 1).getLong(EXPORT_SEQUENCE_NUM));
        }
    }

    @Test
    void headerAndTimestampView() {
        CobolRecord first = records.get(0);
        assertThat(first.get(EXPORT_TIMESTAMP)).isEqualTo("2025-09-28 22:53:40.000000");
        assertThat(first.get(EXPORT_DATE)).isEqualTo("2025-09-28");
        assertThat(first.get(EXPORT_TIME)).isEqualTo("22:53:40.000000");
        assertThat(first.get(EXPORT_BRANCH_ID)).isEqualTo("0001");
        assertThat(first.get(EXPORT_REGION_CODE)).isEqualTo("NORTH");
    }

    @Test
    void customerViewDecodesBinaryIdAndUnsignedPackedFico() {
        CobolRecord customer = records.get(0);
        assertThat(customer.get(EXPORT_REC_TYPE)).isEqualTo("C");
        assertThat(customer.getLong(EXP_CUST_ID)).isEqualTo(1L);
        assertThat(customer.getTrimmed(EXP_CUST_FIRST_NAME)).isEqualTo("IMMANUEL");
        assertThat(customer.getTrimmed(EXP_CUST_ADDR_LINE.get(0))).isEqualTo("618 DESHAUN ROUTE");
        assertThat(customer.getInt(EXP_CUST_FICO_CREDIT_SCORE)).isBetween(300, 850);
        assertThat(customer.toBytes()[EXP_CUST_FICO_CREDIT_SCORE.offset() + 1] & 0x0F).isEqualTo(0x0F);
    }

    @Test
    void accountViewMixesPackedZonedAndBinaryAmounts() {
        CobolRecord account = records.get(50);
        assertThat(account.get(EXPORT_REC_TYPE)).isEqualTo("A");
        assertThat(account.getLong(EXP_ACCT_ID)).isEqualTo(1L);
        assertThat(account.get(EXP_ACCT_ACTIVE_STATUS)).isEqualTo("Y");
        assertThat(EXP_ACCT_CURR_BAL.length()).isEqualTo(7);
        assertThat(account.get(EXP_ACCT_CURR_BAL)).isEqualByComparingTo("0.00");
        assertThat(account.get(EXP_ACCT_CREDIT_LIMIT)).isEqualByComparingTo("2020.00");
        assertThat(account.get(EXP_ACCT_CASH_CREDIT_LIMIT)).isEqualByComparingTo("1020.00");
        assertThat(account.get(EXP_ACCT_OPEN_DATE)).isEqualTo("2020-10-22");
        assertThat(EXP_ACCT_CURR_CYC_DEBIT.length()).isEqualTo(8);
        assertThat(account.get(EXP_ACCT_CURR_CYC_DEBIT)).isEqualByComparingTo("0.00");
    }

    @Test
    void cardXrefAndCardViewsDecodeBinaryAccountIds() {
        CobolRecord xref = records.get(100);
        assertThat(xref.get(EXP_XREF_CARD_NUM)).isEqualTo("0500024453765740");
        assertThat(xref.getLong(EXP_XREF_CUST_ID)).isEqualTo(50L);
        assertThat(xref.getLong(EXP_XREF_ACCT_ID)).isEqualTo(50L);

        CobolRecord card = records.get(450);
        assertThat(card.get(EXPORT_REC_TYPE)).isEqualTo("D");
        assertThat(card.get(EXP_CARD_NUM)).isEqualTo("0500024453765740");
        assertThat(card.getLong(EXP_CARD_ACCT_ID)).isEqualTo(50L);
        assertThat(card.getInt(EXP_CARD_CVV_CD)).isEqualTo(0x02EB);
        assertThat(card.getTrimmed(EXP_CARD_EMBOSSED_NAME)).isEqualTo("Aniya Von");
        assertThat(card.get(EXP_CARD_EXPIRAION_DATE)).isEqualTo("2023-03-09");
        assertThat(card.get(EXP_CARD_ACTIVE_STATUS)).isEqualTo("Y");
    }

    @Test
    void transactionViewDecodesPackedAmount() {
        CobolRecord tran = records.get(150);
        assertThat(tran.get(EXPORT_REC_TYPE)).isEqualTo("T");
        assertThat(tran.get(EXP_TRAN_ID)).isEqualTo("0000000000683580");
        assertThat(tran.get(EXP_TRAN_TYPE_CD)).isEqualTo("01");
        assertThat(tran.getInt(EXP_TRAN_CAT_CD)).isEqualTo(1);
        assertThat(tran.getTrimmed(EXP_TRAN_SOURCE)).isEqualTo("POS TERM");
        assertThat(tran.getTrimmed(EXP_TRAN_DESC)).isEqualTo("Purchase at Abshire-Lowe");
        assertThat(EXP_TRAN_AMT.length()).isEqualTo(6);
        assertThat(tran.get(EXP_TRAN_AMT).scale()).isEqualTo(2);
        assertThat(tran.getTrimmed(EXP_TRAN_MERCHANT_NAME)).isEqualTo("Abshire-Lowe");
    }

    @Test
    void everyRecordDecodesThroughTheViewItsTypeSelects() {
        for (CobolRecord record : records) {
            List<Field> storage = ExportRecordLayout.storageFor(record.get(EXPORT_REC_TYPE));
            assertThat(storage.stream().mapToInt(Field::length).sum()).isEqualTo(500);
            for (Field field : storage) {
                if (field instanceof NumericField numeric) {
                    assertThat(record.isNumeric(numeric)).as("%s in %s", numeric, record).isTrue();
                }
            }
        }
    }

    @Test
    void customerRecordConvertsToAsciiWithoutTouchingBinaryFields() {
        CobolRecord ebcdic = records.get(0);
        CobolRecord ascii = ebcdic.withCodePage(CodePage.ASCII, ExportRecordLayout.storageFor("C"));
        assertThat(ascii.get(EXP_CUST_FIRST_NAME)).isEqualTo(ebcdic.get(EXP_CUST_FIRST_NAME));
        assertThat(ascii.getLong(EXP_CUST_ID)).isEqualTo(1L);
        assertThat(ascii.get(EXP_CUST_FICO_CREDIT_SCORE)).isEqualTo(ebcdic.get(EXP_CUST_FICO_CREDIT_SCORE));
        assertThat(ascii.withCodePage(CodePage.EBCDIC, ExportRecordLayout.storageFor("C")).toBytes())
                .isEqualTo(ebcdic.toBytes());
        assertThatThrownBy(() -> ebcdic.withCodePage(CodePage.ASCII)).hasMessageContaining("REDEFINES");
        assertThat(ebcdic.withCodePage(CodePage.EBCDIC)).isEqualTo(ebcdic);
    }
}
