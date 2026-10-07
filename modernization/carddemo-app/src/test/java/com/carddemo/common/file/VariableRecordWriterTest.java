package com.carddemo.common.file;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class VariableRecordWriterTest {

    @TempDir
    Path dir;

    private static final byte[] AREA = {'H', 'E', 'L', 'L', 'O'};

    @Test
    void framesRecordsPerPrefix() throws IOException {
        assertThat(write(RecordPrefix.NONE)).containsExactly('H', 'E', 'L', 'H', 'E', 'L', 'L', 'O');
        assertThat(write(RecordPrefix.GNUCOBOL_VARSEQ))
                .containsExactly(0, 0, 0, 3, 'H', 'E', 'L', 0, 0, 0, 5, 'H', 'E', 'L', 'L', 'O');
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

    private static void assertStatus(Runnable op, FileStatus status) {
        assertThatThrownBy(op::run).isInstanceOfSatisfying(FileStatusException.class,
                e -> assertThat(e.status()).isEqualTo(status));
    }
}
