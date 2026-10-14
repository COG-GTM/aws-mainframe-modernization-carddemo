package com.carddemo.runtime.vsam;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.runtime.cics.CicsResp;
import com.carddemo.runtime.cobol.PicX;
import java.util.List;
import java.util.stream.Stream;
import org.h2.jdbcx.JdbcDataSource;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

class KsdsFileTest {

    private static int seq;

    static Stream<KsdsStore> stores() {
        JdbcDataSource ds = new JdbcDataSource();
        ds.setURL("jdbc:h2:mem:ksds" + (seq++) + ";DB_CLOSE_DELAY=-1");
        return Stream.of(new InMemoryKsdsStore(), new JdbcKsdsStore(ds, "TEST.KSDS"));
    }

    private static KsdsFile file(KsdsStore store) {
        KsdsFile f = new KsdsFile("FILE1   ", 6, 1, 3, store);
        f.load(List.of("xBBB01", "xAAA01", "xDDD01", "x\u00C9EE01"));
        return f;
    }

    @ParameterizedTest
    @MethodSource("stores")
    void readWriteRewriteDelete(KsdsStore store) {
        KsdsFile f = file(store);
        assertThat(f.name()).isEqualTo("FILE1");
        assertThat(f.read("AAA").dataString()).isEqualTo("xAAA01");
        assertThat(f.read("ZZZ").resp()).isEqualTo(CicsResp.NOTFND);
        assertThat(f.read("ZZZ").resp2()).isEqualTo(80);
        assertThat(f.read("AA").resp()).isEqualTo(CicsResp.INVREQ);
        FileResult shortRead = f.read("AAA", 4);
        assertThat(shortRead.resp()).isEqualTo(CicsResp.LENGERR);
        assertThat(shortRead.dataString()).isEqualTo("xAAA");
        assertThat(f.write("yAAA02").resp()).isEqualTo(CicsResp.DUPREC);
        assertThat(f.write("yCCC02").resp()).isEqualTo(CicsResp.NORMAL);
        assertThat(f.write("short").resp()).isEqualTo(CicsResp.LENGERR);
        assertThat(f.rewrite("zCCC03").resp()).isEqualTo(CicsResp.INVREQ);
        assertThat(f.readForUpdate("CCC").isNormal()).isTrue();
        assertThat(f.rewrite("zAAA03").resp2()).isEqualTo(31);
        assertThat(f.rewrite("zCCC03").isNormal()).isTrue();
        assertThat(f.read("CCC").dataString()).isEqualTo("zCCC03");
        assertThat(f.readForUpdate("CCC").isNormal()).isTrue();
        assertThat(f.deleteHeld().isNormal()).isTrue();
        assertThat(f.delete("CCC").resp()).isEqualTo(CicsResp.NOTFND);
        assertThat(f.readGteq("C").key()).isEqualTo("DDD");
        assertThatThrownBy(() -> f.load(List.of("qAAA"))).isInstanceOf(IllegalArgumentException.class);
        f.setState(FileState.DISABLED);
        assertThat(f.read("AAA").resp()).isEqualTo(CicsResp.DISABLED);
        f.setState(FileState.CLOSED);
        assertThat(f.read("AAA").resp()).isEqualTo(CicsResp.NOTOPEN);
    }

    @ParameterizedTest
    @MethodSource("stores")
    void browseForwardBackwardAndUnsignedKeyOrder(KsdsStore store) {
        KsdsFile f = file(store);
        assertThat(f.readnext().resp()).isEqualTo(CicsResp.INVREQ);
        assertThat(f.startbr("BBA").isNormal()).isTrue();
        assertThat(f.readnext().key()).isEqualTo("BBB");
        assertThat(f.readnext().key()).isEqualTo("DDD");
        assertThat(f.readnext().key()).isEqualTo("\u00C9EE");
        assertThat(f.readnext().resp()).isEqualTo(CicsResp.ENDFILE);
        assertThat(f.endbr().isNormal()).isTrue();

        assertThat(f.startbr("DDD").isNormal()).isTrue();
        assertThat(f.readprev().key()).isEqualTo("DDD");
        assertThat(f.readprev().key()).isEqualTo("BBB");
        assertThat(f.readprev().key()).isEqualTo("AAA");
        assertThat(f.readprev().resp()).isEqualTo(CicsResp.ENDFILE);
        f.endbr();

        assertThat(f.startbr(PicX.highValues(3)).isNormal()).isTrue();
        assertThat(f.readprev().key()).isEqualTo("\u00C9EE");
        f.endbr();

        assertThat(f.startbr("CCC").isNormal()).isTrue();
        assertThat(f.readprev().resp()).isEqualTo(CicsResp.NOTFND);
        assertThat(f.resetbr("AAA", 0).isNormal()).isTrue();
        assertThat(f.readnext().key()).isEqualTo("AAA");
        assertThat(f.readprev().key()).isEqualTo("AAA");
        f.endbr();
        assertThat(f.endbr().resp()).isEqualTo(CicsResp.INVREQ);
        assertThat(f.startbr("\u00F0AA").resp()).isEqualTo(CicsResp.NOTFND);
        assertThat(f.startbr("CCC", true, 1).resp()).isEqualTo(CicsResp.NOTFND);
    }

    @ParameterizedTest
    @MethodSource("stores")
    void alternateIndexPath(KsdsStore store) {
        KsdsFile f = new KsdsFile("CARD", 6, 0, 3, store);
        f.load(List.of("C01X99", "C02X11", "C03Y11"));
        AlternateIndexPath unique = f.path("CARDAIX", 3, 1, true);
        AlternateIndexPath dup = f.path("CARDAIX2", 4, 2, false);
        assertThat(unique.read("Y").dataString()).isEqualTo("C03Y11");
        assertThat(unique.read("Z").resp()).isEqualTo(CicsResp.NOTFND);
        assertThat(dup.read("11").resp()).isEqualTo(CicsResp.DUPKEY);
        assertThat(dup.read("11").dataString()).isEqualTo("C02X11");
        assertThat(dup.startbr("11").isNormal()).isTrue();
        assertThat(dup.readnext().resp()).isEqualTo(CicsResp.DUPKEY);
        FileResult second = dup.readnext();
        assertThat(second.resp()).isEqualTo(CicsResp.NORMAL);
        assertThat(second.dataString()).isEqualTo("C03Y11");
        assertThat(dup.readnext().dataString()).isEqualTo("C01X99");
        assertThat(dup.readnext().resp()).isEqualTo(CicsResp.ENDFILE);
        assertThat(dup.endbr().isNormal()).isTrue();
    }
}
