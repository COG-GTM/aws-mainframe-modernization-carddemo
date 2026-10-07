package com.carddemo.batch.intcalc;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.card.CardXrefRecord;
import com.carddemo.common.codec.RecordEncoding;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class XrefByAccountTest {

    @TempDir
    Path dir;

    private static String line(String card, int cust, long acct) {
        return CardXrefRecord.MAPPER.toRecord(new CardXrefRecord(card, cust, acct), RecordEncoding.ASCII).text();
    }

    @Test
    void returnsTheLowestCardOfTheAccountWhateverTheFileOrder() throws IOException {
        Path file = dir.resolve("cardxref.txt");
        Files.writeString(file, line("4999000000000000", 1, 7) + "\n" + line("4111000000000000", 1, 7) + "\n"
                + line("4555000000000000", 2, 8) + "\n", StandardCharsets.ISO_8859_1);
        XrefByAccount xref = new XrefByAccount("XREFFILE", file, RecordEncoding.ASCII);
        xref.open();
        assertThat(xref.read(7L)).map(CardXrefRecord::cardNum).contains("4111000000000000");
        assertThat(xref.read(8L)).map(CardXrefRecord::cardNum).contains("4555000000000000");
        assertThat(xref.read(9L)).isEmpty();
        xref.close();
    }
}
