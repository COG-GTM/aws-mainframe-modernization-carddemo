package com.carddemo.app;

import com.carddemo.app.data.KsdsDataService.Ksds;
import com.carddemo.app.golden.GoldenCase;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.boot.test.context.SpringBootTest;

import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Golden master (plan §3, FR-POSTTRAN-01..16, PR-CBTRN02C-01..17): every oracle case runs the real {@code POSTTRAN} job
 * through the CLI and must give the oracle's DALYREJS bytes, ACCTDATA / TCATBALF / TRANSACT contents (TRANSACT
 * TRAN-PROC-TS centiseconds masked, R-05; {@code dup-id} tables = atomic end state, D-042), SYSOUT lines and RC
 * (231 ≡ 16, D-039).
 */
@SpringBootTest
class PosttranJobParityTest extends PosttranJobSupport {

    static List<GoldenCase> cases() {
        return GoldenCase.all();
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("cases")
    void javaJobMatchesTheGnuCobolOracle(GoldenCase c, @TempDir Path out) {
        seed(c);
        int rc = launch(c, out, "GOLDEN-" + c.name());
        assertParity(c, out, rc);
        assertThat(data.unload(Ksds.TRANSACT)).noneMatch(r -> r.startsWith(STALE_TRAN_ID));
    }
}
