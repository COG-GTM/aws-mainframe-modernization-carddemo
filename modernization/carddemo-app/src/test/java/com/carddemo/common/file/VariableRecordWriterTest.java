package com.carddemo.common.file;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

class VariableRecordWriterTest {

    @TempDir
    Path dir;

    private static final byte[] AREA = {'H', 'E', 'L', 'L', 'O'};

    @Test
    void framesRecordsPerPrefix() throws IOException {
        assertThat(write(RecordPrefix.NONE)).containsExactly('H', 'E', 'L', 'H', 'E', 'L', 'L', 'O');
        assertThat(write(RecordPrefix.GNUCOBOL_VARSEQ))
                .containsExactly(0, 0, 0, 3, 'H', 'E', 'L', 0, 0, 0, 5, 'H', 'E', 'L', 'L', 'O');
        assertThat(write(RecordPrefix.GNUCOBOL_VARSEQ_0))
                .containsExactly(0, 3, 0, 0, 'H', 'E', 'L', 0, 5, 0, 0, 'H', 'E', 'L', 'L', 'O');
        assertThat(write(RecordPrefix.ZOS_RDW))
                .containsExactly(0, 7, 0, 0, 'H', 'E', 'L', 0, 9, 0, 0, 'H', 'E', 'L', 'L', 'O');
    }

    private byte[] write(RecordPrefix prefix) throws IOException {
        Path file = dir.resolve(prefix.name());
        VariableRecordWriter w = new VariableRecordWriter("OUTFILE", file, prefix, 1, 5);
        assertThat(w.isOpen()).isFalse();
        w.open();
        assertThat(w.isOpen()).isTrue();
        w.write(AREA, 3);
        w.write(AREA, 5);
        w.close();
        return Files.readAllBytes(file);
    }

    @Test
    void lifecycleErrorsCarryCobolStatuses() {
        VariableRecordWriter w = new VariableRecordWriter("OUTFILE", dir.resolve("f"), RecordPrefix.NONE, 2, 4);
        assertStatus(() -> w.write(AREA, 3), FileStatus.NOT_OPEN_OUTPUT);
        assertStatus(w::close, FileStatus.NOT_OPEN);
        w.open();
        assertStatus(w::open, FileStatus.ALREADY_OPEN);
        assertStatus(() -> w.write(AREA, 1), FileStatus.RECORD_LENGTH_ERROR);
        assertStatus(() -> w.write(AREA, 5), FileStatus.RECORD_LENGTH_ERROR);
        assertStatus(() -> w.write(new byte[2], 3), FileStatus.RECORD_LENGTH_ERROR);
        w.close();
        assertStatus(() -> new VariableRecordWriter("X", dir.resolve("no/such/dir/f"), RecordPrefix.NONE, 1, 2).open(),
                FileStatus.FILE_NOT_FOUND);
        assertStatus(() -> new VariableRecordWriter("X", dir, RecordPrefix.NONE, 1, 2).open(),
                FileStatus.PERMANENT_ERROR);
        assertThatThrownBy(() -> new VariableRecordWriter("X", dir, RecordPrefix.NONE, 3, 2))
                .isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> new VariableRecordWriter("X", dir, RecordPrefix.NONE, -1, 2))
                .isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void rdwCannotFrameMoreThan65531Bytes() {
        new VariableRecordWriter("X", dir.resolve("r"), RecordPrefix.ZOS_RDW, 1, VariableRecordWriter.MAX_RDW_PAYLOAD);
        new VariableRecordWriter("X", dir.resolve("v"), RecordPrefix.GNUCOBOL_VARSEQ, 1, 70_000);
        assertThatThrownBy(() -> new VariableRecordWriter("X", dir.resolve("r"), RecordPrefix.ZOS_RDW, 1,
                VariableRecordWriter.MAX_RDW_PAYLOAD + 1)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void readOnlyTargetIsOpenModeNotAllowed() throws IOException {
        Path file = Files.createFile(dir.resolve("ro"));
        assumeTrue(file.toFile().setWritable(false) && !Files.isWritable(file), "running as root");
        assertStatus(() -> new VariableRecordWriter("X", file, RecordPrefix.NONE, 1, 2).open(),
                FileStatus.OPEN_MODE_NOT_ALLOWED);
    }

    @Test
    void deviceErrorsArePermanentErrors() {
        Path full = Path.of("/dev/full");
        assumeTrue(Files.isWritable(full), "/dev/full not available");
        VariableRecordWriter direct = new VariableRecordWriter("X", full, RecordPrefix.NONE, 1, 20_000);
        direct.open();
        assertStatus(() -> direct.write(new byte[20_000], 20_000), FileStatus.PERMANENT_ERROR);
        VariableRecordWriter buffered = new VariableRecordWriter("X", full, RecordPrefix.NONE, 1, 3);
        buffered.open();
        buffered.write(AREA, 3);
        assertStatus(buffered::close, FileStatus.PERMANENT_ERROR);
        assertThat(buffered.isOpen()).isFalse();
    }

    private static void assertStatus(Runnable op, FileStatus status) {
        assertThatThrownBy(op::run).isInstanceOfSatisfying(FileStatusException.class,
                e -> assertThat(e.status()).isEqualTo(status));
    }
}
