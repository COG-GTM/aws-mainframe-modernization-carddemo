package com.carddemo.batch.load;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.codec.TestData;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.testcontainers.service.connection.ServiceConnection;
import org.springframework.jdbc.core.JdbcTemplate;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

/** A REPLACE rerun whose parent keys differ from the stored rows replaces parents and children alike. */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE)
@Testcontainers
class InitialLoadKeyChangeIT {

    static final Charset EBCDIC = Charset.forName("IBM037");

    @Container
    @ServiceConnection
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine");

    @Autowired
    JobLauncher jobLauncher;
    @Autowired
    @Qualifier("initialLoadJob")
    Job initialLoadJob;
    @Autowired
    JdbcTemplate jdbc;

    @TempDir
    Path source;

    @Test
    void replaceRerunWithRenumberedCustomerSucceeds() throws Exception {
        Path samples = TestData.resolve("app/data/EBCDIC");
        for (InitialLoadInput input : InitialLoadInput.values()) {
            Files.copy(samples.resolve(input.fileName()), source.resolve(input.fileName()));
        }
        assertThat(run(LoadMode.REPLACE)).isEqualTo(BatchStatus.COMPLETED);
        long xrefsOfCustomer1 = count("select count(*) from card_xref where cust_id = 1");
        assertThat(xrefsOfCustomer1).isPositive();

        renumberCustomer(InitialLoadInput.CUSTDATA, 500, 0);
        renumberCustomer(InitialLoadInput.CARDXREF, 50, 16);

        assertThat(run(LoadMode.REPLACE)).isEqualTo(BatchStatus.COMPLETED);
        assertThat(count("select count(*) from customer where cust_id = 1")).isZero();
        assertThat(count("select count(*) from customer where cust_id = 999")).isEqualTo(1);
        assertThat(count("select count(*) from card_xref where cust_id = 999")).isEqualTo(xrefsOfCustomer1);
        assertThat(count("select count(*) from customer")).isEqualTo(50);
        assertThat(count("select count(*) from card_xref")).isEqualTo(50);
    }

    /** Customer id 000000001 becomes 000000999 in the PIC 9(09) field at {@code offset} of every record. */
    private void renumberCustomer(InitialLoadInput input, int lrecl, int offset) throws IOException {
        Path file = source.resolve(input.fileName());
        byte[] data = Files.readAllBytes(file);
        byte[] from = "000000001".getBytes(EBCDIC);
        byte[] to = "000000999".getBytes(EBCDIC);
        int changed = 0;
        for (int at = offset; at + from.length <= data.length; at += lrecl) {
            if (Arrays.equals(data, at, at + from.length, from, 0, from.length)) {
                System.arraycopy(to, 0, data, at, to.length);
                changed++;
            }
        }
        assertThat(changed).as(input + " records renumbered").isPositive();
        Files.write(file, data);
    }

    private BatchStatus run(LoadMode mode) throws Exception {
        return jobLauncher.run(initialLoadJob, InitialLoadJobConfiguration.parameters(source, mode)).getStatus();
    }

    private long count(String sql) {
        return jdbc.queryForObject(sql, Long.class);
    }
}
