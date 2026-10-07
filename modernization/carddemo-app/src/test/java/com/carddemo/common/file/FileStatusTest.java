package com.carddemo.common.file;

import com.carddemo.common.AbendException;
import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class FileStatusTest {

    @Test
    void codesAreUniqueTwoDigitStatusKeys() {
        assertThat(Arrays.stream(FileStatus.values()).map(FileStatus::code)).doesNotHaveDuplicates()
                .allMatch(c -> c.matches("\\d\\d"));
        for (FileStatus s : FileStatus.values()) {
            assertThat(FileStatus.of(s.code())).isSameAs(s);
        }
        assertThatThrownBy(() -> FileStatus.of("XX")).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void classifiesByStatusKeyOne() {
        assertThat(FileStatus.SUCCESS.isSuccessful()).isTrue();
        assertThat(FileStatus.DUPLICATE_ALTERNATE_KEY.isSuccessful()).isTrue();
        assertThat(FileStatus.END_OF_FILE.isEndOfFile()).isTrue();
        assertThat(FileStatus.END_OF_FILE.isSuccessful()).isFalse();
        assertThat(FileStatus.RECORD_NOT_FOUND.isInvalidKey()).isTrue();
        assertThat(FileStatus.FILE_NOT_FOUND.isPermanentError()).isTrue();
        assertThat(FileStatus.NOT_OPEN.isLogicError()).isTrue();
        assertThat(FileStatus.VSAM_LOGIC_ERROR.isImplementorDefined()).isTrue();
        assertThat(FileStatus.SUCCESS.isEndOfFile()).isFalse();
        assertThat(FileStatus.DUPLICATE_KEY.toString()).isEqualTo("22 DUPLICATE_KEY");
    }

    @Test
    void displaysIoStatusLikeTheBatchPrograms() {
        assertThat(FileStatus.FILE_NOT_FOUND.displayIoStatus()).isEqualTo("FILE STATUS IS: NNNN0035");
        assertThat(FileStatus.VSAM_LOGIC_ERROR.displayIoStatus()).isEqualTo("FILE STATUS IS: NNNN9050");
        assertThat(FileStatus.displayIoStatus("9\u0012")).isEqualTo("FILE STATUS IS: NNNN9018");
        assertThat(FileStatus.displayIoStatus("A1")).isEqualTo("FILE STATUS IS: NNNNA049");
        assertThatThrownBy(() -> FileStatus.displayIoStatus("123")).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void fileStatusExceptionIsTheCardDemoAbend() {
        FileStatusException e = new FileStatusException("ACCTFILE", "READING", FileStatus.RECORD_NOT_FOUND);
        assertThat(e).isInstanceOf(AbendException.class);
        assertThat(e.abendCode()).isEqualTo(AbendException.CARDDEMO_ABEND_CODE);
        assertThat(e.ddname()).isEqualTo("ACCTFILE");
        assertThat(e.operation()).isEqualTo("READING");
        assertThat(e.status()).isSameAs(FileStatus.RECORD_NOT_FOUND);
        assertThat(e.getMessage()).isEqualTo("USER ABEND U0999: ERROR READING ACCTFILE FILE STATUS 23");
    }
}
