package com.carddemo.batch;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.data.domain.Limit;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

/** Generation files stay aligned with the batch_output_file catalog when its transaction commits or rolls back. */
class DatedOutputFilesTest {

    @TempDir
    Path dir;

    BatchOutputFileRepository repository = mock(BatchOutputFileRepository.class);
    DatedOutputFiles outputs;
    Path expired;
    BatchOutputFile expiredRow;

    @BeforeEach
    void setUp() throws IOException {
        Clock clock = Clock.fixed(Instant.parse("2022-07-06T00:00:00Z"), ZoneOffset.UTC);
        outputs = new DatedOutputFiles(repository, new BatchOutputProperties(dir, 1), clock);
        expired = Files.writeString(dir.resolve("CUSTDATA.IMPORT.2022-07-05.1"), "old");
        expiredRow = new BatchOutputFile("CUSTDATA.IMPORT", LocalDate.of(2022, 7, 5), 1, expired.toString(), 0,
                "0".repeat(64));
        List<BatchOutputFile> saved = new ArrayList<>();
        when(repository.save(any())).thenAnswer(i -> {
            saved.add(0, i.getArgument(0));
            return i.getArgument(0);
        });
        when(repository.findByGdgBaseOrderByBusinessDateDescJobExecutionIdDesc("CUSTDATA.IMPORT", Limit.unlimited()))
                .thenAnswer(i -> {
                    List<BatchOutputFile> newestFirst = new ArrayList<>(saved);
                    newestFirst.add(expiredRow);
                    return newestFirst;
                });
        TransactionSynchronizationManager.initSynchronization();
    }

    @AfterEach
    void tearDown() {
        TransactionSynchronizationManager.clearSynchronization();
    }

    @Test
    void expiredGenerationIsDeletedOnlyAfterCommit() {
        BatchOutputFile written = outputs.write("CUSTDATA.IMPORT", 2, List.of());

        verify(repository).delete(expiredRow);
        assertThat(expired).exists();
        complete(true);
        assertThat(expired).doesNotExist();
        assertThat(Path.of(written.getFilePath())).exists();
    }

    @Test
    void generationIsNamedAndCataloguedByTheGivenBusinessDate() {
        BatchOutputFile written = outputs.write("DALYREJS", java.time.LocalDate.parse("2022-01-31"), 3, List.of());

        assertThat(written.getBusinessDate()).isEqualTo(java.time.LocalDate.parse("2022-01-31"));
        assertThat(Path.of(written.getFilePath()).getFileName()).hasToString("DALYREJS.2022-01-31.3");
    }

    @Test
    void rollbackKeepsExpiredGenerationAndRemovesNewFile() {
        BatchOutputFile written = outputs.write("CUSTDATA.IMPORT", 2, List.of());

        complete(false);
        assertThat(expired).exists();
        assertThat(Path.of(written.getFilePath())).doesNotExist();
    }

    private static void complete(boolean commit) {
        List<TransactionSynchronization> synchronizations = TransactionSynchronizationManager.getSynchronizations();
        for (TransactionSynchronization s : synchronizations) {
            if (commit) {
                s.afterCommit();
            }
            s.afterCompletion(commit ? TransactionSynchronization.STATUS_COMMITTED
                    : TransactionSynchronization.STATUS_ROLLED_BACK);
        }
    }
}
