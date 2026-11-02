package com.carddemo.batch.tranrept;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.Test;

/** Against GnuCOBOL's MOVE to the two CVTRA07Y pictures ({@code scripts/migration/tranrept/edited-amounts.sh}). */
class EditedAmountTest {

    private static final Path DIR = Path.of("src/test/resources/tranrept-edited-amounts");

    @Test
    void matchesGnuCobolForEveryValue() throws IOException {
        List<String> expected = Files.readAllLines(DIR.resolve("expected.txt"), StandardCharsets.US_ASCII);
        assertThat(expected).hasSize(Files.readAllLines(DIR.resolve("AMOUNTS.txt")).size());
        for (String line : expected) {
            String[] parts = line.split("\\|", -1);
            BigDecimal value = new BigDecimal(parts[0]).movePointLeft(2);
            assertThat(EditedAmount.detail(value)).as("-ZZZ,ZZZ,ZZZ.ZZ of %s", value).isEqualTo(parts[1]);
            assertThat(EditedAmount.total(value)).as("+ZZZ,ZZZ,ZZZ.ZZ of %s", value).isEqualTo(parts[2]);
        }
    }

    @Test
    void totalsKeepTheLowOrderDigitsOnOverflow() {
        assertThat(TransactionReportProgram.add(new BigDecimal("999999999.99"), new BigDecimal("0.02")))
                .isEqualByComparingTo("0.01");
        assertThat(TransactionReportProgram.add(new BigDecimal("-999999999.99"), new BigDecimal("-999999999.99")))
                .isEqualByComparingTo("-999999999.98");
    }
}
