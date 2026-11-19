package com.carddemo.common.error;

import com.carddemo.common.batch.ReturnCode;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class FileStatusTest {

    @Test
    void okAndExpectedStatusesPass() {
        assertThatCode(() -> FileStatus.check(FileStatus.OK, "TRANFILE", "WRITE")).doesNotThrowAnyException();
        assertThatCode(() -> FileStatus.check(FileStatus.END_OF_FILE, "DALYTRAN", "READ", FileStatus.END_OF_FILE))
                .doesNotThrowAnyException();
        assertThatCode(() -> FileStatus.check(FileStatus.RECORD_NOT_FOUND, "TCATBALF", "READ", FileStatus.RECORD_NOT_FOUND))
                .doesNotThrowAnyException();
    }

    @Test
    void unexpectedStatusIsATypedAbendWithRc16() {
        assertThatThrownBy(() -> FileStatus.check(FileStatus.DUPLICATE_KEY, "TRANFILE", "WRITE"))
                .isInstanceOf(DuplicateKeyException.class)
                .isInstanceOf(AbendException.class)
                .hasMessage("WRITE TRANFILE failed: FILE STATUS 22")
                .satisfies(ex -> {
                    AbendException abend = (AbendException) ex;
                    assertThat(abend.returnCode()).isEqualTo(ReturnCode.ABEND);
                    assertThat(abend.abendCode()).isEqualTo(999);
                });
        assertThatThrownBy(() -> FileStatus.check(FileStatus.RECORD_NOT_FOUND, "ACCTFILE", "READ"))
                .isInstanceOf(RecordNotFoundException.class);
        assertThatThrownBy(() -> FileStatus.check(FileStatus.END_OF_FILE, "XREFFILE", "READ"))
                .isInstanceOf(UnexpectedEndOfFileException.class);
        assertThatThrownBy(() -> FileStatus.check(FileStatus.of("35"), "DALYTRAN", "OPEN"))
                .isExactlyInstanceOf(FileStatusException.class)
                .satisfies(ex -> assertThat(((FileStatusException) ex).ddName()).isEqualTo("DALYTRAN"));
        // Successful-with-condition statuses are not silently accepted either.
        assertThatThrownBy(() -> FileStatus.check(FileStatus.RECORD_LENGTH_CONFLICT, "DALYTRAN", "READ"))
                .isInstanceOf(FileStatusException.class);
    }

    @Test
    void displayLineMatches9910DisplayIoStatus() {
        // R-06: duplicate write shows 'FILE STATUS IS: NNNN0022'.
        assertThat(FileStatus.DUPLICATE_KEY.displayLine()).isEqualTo("FILE STATUS IS: NNNN0022");
        assertThat(FileStatus.of("35").ioStatus04()).isEqualTo("0035");
        assertThat(FileStatus.of("9#").ioStatus04()).isEqualTo("9035");
    }

    @Test
    void statusIsTwoBytes() {
        assertThatThrownBy(() -> FileStatus.of("0")).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> FileStatus.of(null)).isInstanceOf(NullPointerException.class);
        assertThat(FileStatus.of("00").isOk()).isTrue();
    }
}
