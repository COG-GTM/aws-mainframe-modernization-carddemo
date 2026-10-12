package com.carddemo.posting;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.boot.SpringApplication;
import org.springframework.context.ConfigurableApplicationContext;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * The process exit code is the JCL scheduler contract of CBTRN02C:
 * 0 = all posted, 4 = rejects written (RETURN-CODE set by {@code IF WS-REJECT-COUNT > 0 MOVE 4 TO
 * RETURN-CODE}), non-zero = abend. These cases run the real {@link PostingApplication} process
 * entry path (plain {@code SpringApplication.run} + {@code SpringApplication.exit}), not a test
 * launcher, so the whole exit-code chain is exercised.
 */
class PostingProcessExitCodeTest {

    static final Path DATA = Path.of("../../../app/data/EBCDIC").toAbsolutePath().normalize();

    @Test
    void exits4WhenRejectsAreWritten(@TempDir Path work) throws IOException {
        stageMasters(work);
        assertThat(exitCode(work, "--carddemo.posting.daily-transactions=" + DATA.resolve("AWS.M2.CARDDEMO.DALYTRAN.PS")))
                .isEqualTo(4);
    }

    @Test
    void exits0WhenNoRejectsAreWritten(@TempDir Path work) throws IOException {
        stageMasters(work);
        Files.write(work.resolve("DALYTRAN"), new byte[0]);
        assertThat(exitCode(work, "--carddemo.posting.daily-transactions=" + work.resolve("DALYTRAN")))
                .isEqualTo(0);
    }

    @Test
    void exitsNonZeroWhenOutputCannotBeWritten(@TempDir Path work) throws IOException {
        stageMasters(work);
        Files.write(work.resolve("not-a-directory"), new byte[0]);
        assertThat(exitCode(work,
                "--carddemo.posting.daily-transactions=" + DATA.resolve("AWS.M2.CARDDEMO.DALYTRAN.PS"),
                "--carddemo.posting.transaction-master=" + work.resolve("not-a-directory").resolve("TRANSACT")))
                .isNotZero();
    }

    private static void stageMasters(Path work) throws IOException {
        Files.copy(DATA.resolve("AWS.M2.CARDDEMO.ACCTDATA.PS"), work.resolve("ACCTDATA"), StandardCopyOption.REPLACE_EXISTING);
        Files.copy(DATA.resolve("AWS.M2.CARDDEMO.TCATBALF.PS"), work.resolve("TCATBALF"), StandardCopyOption.REPLACE_EXISTING);
    }

    private static int exitCode(Path work, String... overrides) {
        String[] args = new String[]{
                "--spring.datasource.url=jdbc:h2:mem:exit-" + UUID.randomUUID() + ";DB_CLOSE_DELAY=-1",
                "--carddemo.posting.encoding=EBCDIC",
                "--carddemo.posting.card-xref=" + DATA.resolve("AWS.M2.CARDDEMO.CARDXREF.PS"),
                "--carddemo.posting.account-master=" + work.resolve("ACCTDATA"),
                "--carddemo.posting.category-balances=" + work.resolve("TCATBALF"),
                "--carddemo.posting.transaction-master=" + work.resolve("TRANSACT"),
                "--carddemo.posting.rejects=" + work.resolve("DALYREJS"),
        };
        String[] all = new String[args.length + overrides.length];
        System.arraycopy(args, 0, all, 0, args.length);
        System.arraycopy(overrides, 0, all, args.length, overrides.length);
        try (ConfigurableApplicationContext ctx = SpringApplication.run(PostingApplication.class, all)) {
            return SpringApplication.exit(ctx);
        }
    }
}
