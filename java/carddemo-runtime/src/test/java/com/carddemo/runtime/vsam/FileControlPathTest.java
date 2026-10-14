package com.carddemo.runtime.vsam;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.runtime.cics.CicsResp;
import com.carddemo.runtime.cobol.PicX;
import java.util.List;
import org.junit.jupiter.api.Test;

class FileControlPathTest {

    private final KsdsFile base = KsdsFile.inMemory("BASE", 10, 0, 4);
    private final FileControl fct = new FileControl().register(base).register(base.path("BYALT", 4, 3, false));

    FileControlPathTest() {
        base.load(List.of("0001AAAxxx", "0002BBByyy", "0003BBBzzz"));
    }

    @Test
    void readsThroughRegisteredPath() {
        FileResult r = fct.read("BYALT   ", "AAA", 10);
        assertThat(r.resp()).isEqualTo(CicsResp.NORMAL);
        assertThat(PicX.fromBytes(r.data())).isEqualTo("0001AAAxxx");
        assertThat(fct.path("BYALT")).isPresent();
    }

    @Test
    void pathNotFoundAndDuplicates() {
        assertThat(fct.read("BYALT", "ZZZ", 10).resp()).isEqualTo(CicsResp.NOTFND);
        assertThat(fct.read("BYALT", "ZZZ", 10).resp2()).isEqualTo(80);
        FileResult dup = fct.read("BYALT", "BBB", 10);
        assertThat(dup.resp()).isEqualTo(CicsResp.DUPKEY);
        assertThat(PicX.fromBytes(dup.data())).isEqualTo("0002BBByyy");
    }

    @Test
    void baseFileStillWinsAndUnknownIsFileNotFound() {
        assertThat(fct.read("BASE", "0003", 10).resp()).isEqualTo(CicsResp.NORMAL);
        assertThat(fct.read("NOPE", "0003", 10).resp()).isEqualTo(CicsResp.FILENOTFOUND);
    }
}
