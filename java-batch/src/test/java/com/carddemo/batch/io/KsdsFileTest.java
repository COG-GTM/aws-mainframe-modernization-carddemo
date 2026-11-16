package com.carddemo.batch.io;

import com.carddemo.batch.model.TranCatBalRecord;
import com.carddemo.batch.model.TranCatBalRecord.TranCatKey;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class KsdsFileTest {

    @Test
    void loadsReadsRewritesAndSavesInKeyOrder(@TempDir Path tmp) throws Exception {
        KsdsFile<TranCatKey, TranCatBalRecord> tcat = KsdsFile.load(
                CardDemoData.file(CardDemoData.TRAN_CAT_BALANCES), TranCatBalRecord.CODEC,
                TranCatBalRecord::tranCatKey);
        assertThat(tcat.size()).isEqualTo(50);

        TranCatKey key = new TranCatKey(1, "01", 1);
        TranCatBalRecord rec = tcat.read(key).orElseThrow();
        tcat.rewrite(rec.withTranCatBal(new BigDecimal("-12.34")));
        assertThat(tcat.read(key).orElseThrow().tranCatBal()).isEqualByComparingTo("-12.34");

        TranCatKey newKey = new TranCatKey(0, "01", 1);
        assertThat(tcat.read(newKey)).isEmpty();
        assertThatThrownBy(() -> tcat.rewrite(new TranCatBalRecord(newKey, BigDecimal.ZERO, " ".repeat(22))))
                .isInstanceOf(IllegalStateException.class);
        tcat.write(new TranCatBalRecord(newKey, new BigDecimal("1.00"), " ".repeat(22)));
        assertThatThrownBy(() -> tcat.write(rec)).isInstanceOf(IllegalStateException.class);

        Path out = tmp.resolve("tcatbal.txt");
        tcat.save(out);
        List<String> lines = Files.readAllLines(out);
        assertThat(lines).hasSize(51);
        assertThat(lines.get(0)).startsWith("00000000000010001").contains("0000000010{");
        assertThat(lines.get(1)).startsWith("00000000001010001").contains("0000000123M");
        assertThat(lines).isSorted();
    }
}
