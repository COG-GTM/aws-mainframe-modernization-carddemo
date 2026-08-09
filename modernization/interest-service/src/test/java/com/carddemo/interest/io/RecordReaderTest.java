package com.carddemo.interest.io;

import com.carddemo.interest.batch.InterestDatasets;
import com.carddemo.interest.domain.Account;
import com.carddemo.interest.domain.DisclosureGroup;
import com.carddemo.interest.io.codec.AccountCodec;
import com.carddemo.interest.io.layout.CardDemoLayouts;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

/** Verifies the reusable EBCDIC / copybook record reader against the shipped datasets. */
class RecordReaderTest {

    private static final Path EBCDIC_DIRECTORY = Path.of("..", "..", "app", "data", "EBCDIC");

    @Test
    @DisplayName("Every shipped account record survives a decode/encode round trip byte for byte")
    void accountRecordsRoundTrip() {
        List<byte[]> records = FixedLengthRecordReader.readRecords(
                EBCDIC_DIRECTORY.resolve(InterestDatasets.ACCTFILE_DATASET), AccountCodec.recordLength());
        assertEquals(50, records.size());
        for (byte[] record : records) {
            Account account = AccountCodec.decode(record);
            assertArrayEquals(record, AccountCodec.encode(account),
                    "round trip must be lossless for account " + account.id());
        }
    }

    @Test
    @DisplayName("Zoned decimal overpunch signs decode to exact fixed-point values")
    void zonedDecimalDecoding() {
        byte[] record = EbcdicText.toEbcdic("0000000194J");
        assertEquals(new BigDecimal("-19.41"), ZonedDecimalCodec.decode(record, 0, 11, 2));

        byte[] positive = EbcdicText.toEbcdic("0000000194{");
        assertEquals(new BigDecimal("19.40"), ZonedDecimalCodec.decode(positive, 0, 11, 2));
    }

    @Test
    @DisplayName("Packed decimal (COMP-3) round trips positive and negative values")
    void packedDecimalRoundTrip() {
        byte[] record = new byte[PackedDecimalCodec.byteLength(11)];
        PackedDecimalCodec.encode(record, 0, 11, 2, new BigDecimal("-12345.67"));
        assertEquals(new BigDecimal("-12345.67"), PackedDecimalCodec.decode(record, 0, 11, 2));

        PackedDecimalCodec.encode(record, 0, 11, 2, new BigDecimal("98765.43"));
        assertEquals(new BigDecimal("98765.43"), PackedDecimalCodec.decode(record, 0, 11, 2));
    }

    @Test
    @DisplayName("Copybook layouts declare exactly the copybook record lengths")
    void layoutLengths() {
        assertEquals(50, CardDemoLayouts.TRANSACTION_CATEGORY_BALANCE.recordLength());
        assertEquals(50, CardDemoLayouts.CARD_XREF.recordLength());
        assertEquals(50, CardDemoLayouts.DISCLOSURE_GROUP.recordLength());
        assertEquals(300, CardDemoLayouts.ACCOUNT.recordLength());
        assertEquals(350, CardDemoLayouts.TRANSACTION.recordLength());
    }

    @Test
    @DisplayName("Disclosure group rates decode as annual percentages")
    void disclosureGroupRates() {
        List<DisclosureGroup> groups = InterestDatasets.fromDirectory(EBCDIC_DIRECTORY).disclosureGroups();
        assertEquals(51, groups.size());
        DisclosureGroup defaultFirstCategory = groups.stream()
                .filter(group -> group.key().accountGroupId().equals("DEFAULT")
                        && group.key().category().categoryCode() == 1
                        && group.key().category().typeCode().equals("01"))
                .findFirst().orElseThrow();
        assertEquals(new BigDecimal("15.00"), defaultFirstCategory.annualRatePercent());
    }
}
