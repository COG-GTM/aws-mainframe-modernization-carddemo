package com.carddemo.common.batch;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class SysoutWriterTest {

    @Test
    void linesGoToStdoutAndAreAppendedToTheSysoutDataset(@TempDir Path tmp) throws IOException {
        Path file = tmp.resolve("SYSOUT");
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        new SysoutWriter(file, new PrintStream(out, true, StandardCharsets.UTF_8)).println("FIRST RUN");
        SysoutWriter restart = new SysoutWriter(file, new PrintStream(out, true, StandardCharsets.UTF_8));
        restart.println("SECOND RUN");
        assertThat(Files.readAllLines(file)).containsExactly("FIRST RUN", "SECOND RUN");
        assertThat(out.toString(StandardCharsets.UTF_8)).contains("FIRST RUN", "SECOND RUN");
    }

    @Test
    void abendPrintsErrorStatusAndAbendingProgramOnce(@TempDir Path tmp) throws IOException {
        Path file = tmp.resolve("SYSOUT");
        SysoutWriter sysout = new SysoutWriter(file, new PrintStream(new ByteArrayOutputStream(), true, StandardCharsets.UTF_8));
        sysout.abend("ERROR WRITING TO TRANSACTION FILE", "22");
        sysout.abend();
        assertThat(sysout.abended()).isTrue();
        assertThat(Files.readAllLines(file)).containsExactly("ERROR WRITING TO TRANSACTION FILE", "FILE STATUS IS: NNNN0022",
                "ABENDING PROGRAM");
    }

    @Test
    void fileStatusLineFollows9910DisplayIoStatusForNumericStatuses() {
        assertThat(SysoutWriter.fileStatusLine("35")).isEqualTo("FILE STATUS IS: NNNN0035");
        assertThatThrownBy(() -> SysoutWriter.fileStatusLine("9A")).isInstanceOf(IllegalArgumentException.class);
    }
}
