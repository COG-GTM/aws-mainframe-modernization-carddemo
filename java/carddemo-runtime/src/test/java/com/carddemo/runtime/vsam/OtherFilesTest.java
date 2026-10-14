package com.carddemo.runtime.vsam;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.runtime.cics.CicsResp;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import org.junit.jupiter.api.Test;

class OtherFilesTest {

    @Test
    void esdsAppendsByRba() {
        EsdsFile f = new EsdsFile("LOG");
        assertThat(f.write("AAAA").recordId()).isZero();
        assertThat(f.write("BB").recordId()).isEqualTo(4);
        assertThat(f.read(4).dataString()).isEqualTo("BB");
        assertThat(f.read(3).resp()).isEqualTo(CicsResp.NOTFND);
        assertThat(f.rewrite(4, "CCC").resp()).isEqualTo(CicsResp.LENGERR);
        assertThat(f.rewrite(4, "CC").isNormal()).isTrue();
        assertThat(f.delete(0).resp()).isEqualTo(CicsResp.INVREQ);
        assertThat(f.readAll()).containsExactly("AAAA", "CC");
    }

    @Test
    void rrdsSlots() {
        RrdsFile f = new RrdsFile("REL", 2);
        assertThat(f.write(3, "AB").isNormal()).isTrue();
        assertThat(f.write(3, "CD").resp()).isEqualTo(CicsResp.DUPREC);
        assertThat(f.write(0, "CD").resp()).isEqualTo(CicsResp.INVREQ);
        assertThat(f.read(1).resp()).isEqualTo(CicsResp.NOTFND);
        assertThat(f.readNextFrom(1).recordId()).isEqualTo(3);
        assertThat(f.rewrite(3, "XY").isNormal()).isTrue();
        assertThat(f.read(3).dataString()).isEqualTo("XY");
        assertThat(f.delete(3).isNormal()).isTrue();
        assertThat(f.readNextFrom(1).resp()).isEqualTo(CicsResp.ENDFILE);
    }

    @Test
    void fileControlUnknownFile() {
        FileControl fct = new FileControl();
        assertThat(fct.read("NOPE", "K", 1).resp()).isEqualTo(CicsResp.FILENOTFOUND);
        assertThat(fct.write("NOPE", "K").resp()).isEqualTo(CicsResp.FILENOTFOUND);
        fct.register(KsdsFile.inMemory("F", 2, 0, 1));
        assertThat(fct.write("F  ", "A1").isNormal()).isTrue();
        assertThat(fct.startbr("F", "A").isNormal()).isTrue();
        assertThat(fct.readnext("F").dataString()).isEqualTo("A1");
        assertThat(fct.readprev("F").dataString()).isEqualTo("A1");
        assertThat(fct.endbr("F").isNormal()).isTrue();
        assertThat(fct.readForUpdate("F", "A").isNormal()).isTrue();
        assertThat(fct.rewrite("F", "A2").isNormal()).isTrue();
        assertThat(fct.delete("F", "A").isNormal()).isTrue();
    }

    @Test
    void flatFiles() {
        assertThat(FlatFiles.readLines(new ByteArrayInputStream("AB\r\nCDEFG\n".getBytes(StandardCharsets.ISO_8859_1)), 4))
                .containsExactly("AB  ", "CDEF");
        assertThat(FlatFiles.readFixed("AABBC".getBytes(StandardCharsets.ISO_8859_1), 2)).containsExactly("AA", "BB");
    }
}
