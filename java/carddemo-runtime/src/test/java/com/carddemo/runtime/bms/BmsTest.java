package com.carddemo.runtime.bms;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.runtime.cobol.PicX;
import java.util.Map;
import org.junit.jupiter.api.Test;

class BmsTest {

    private static final String SAMPLE = String.join("\n",
            "* comment",
            "TSTSET  DFHMSD LANG=COBOL,MODE=INOUT,TYPE=&&SYSPARM",
            "TSTMAP  DFHMDI SIZE=(24,80)",
            "        DFHMDF ATTRB=(ASKIP,NORM),POS=(1,1),LENGTH=5,INITIAL='Tran:'",
            "NAME    DFHMDF ATTRB=(UNPROT,IC),POS=(2,10),LENGTH=6",
            "SECRET  DFHMDF ATTRB=(DRK,UNPROT),POS=(3,10),LENGTH=4",
            "AMT     DFHMDF POS=(4,10),LENGTH=5,JUSTIFY=(RIGHT,ZERO),ATTRB=UNPROT",
            "MSG     DFHMDF ATTRB=(ASKIP),POS=(24,1),LENGTH=10,INITIAL='It''s ok'",
            "        DFHMSD TYPE=FINAL",
            "        END");

    @Test
    void parsesSourceIncludingContinuationLinesOfTheRealCosgn00Mapset() {
        BmsMapsetDefinition set = BmsMapsetRegistry.mapset("COSGN00");
        BmsMapDefinition map = set.map("COSGN0A");
        assertThat(set.name()).isEqualTo("COSGN00");
        assertThat(map.rows()).isEqualTo(24);
        assertThat(map.columns()).isEqualTo(80);
        assertThat(map.namedFields()).extracting(BmsFieldDefinition::name).containsExactly("TRNNAME", "TITLE01",
                "CURDATE", "PGMNAME", "TITLE02", "CURTIME", "APPLID", "SYSID", "USERID", "PASSWD", "ERRMSG");
        BmsFieldDefinition userid = map.field("USERID").orElseThrow();
        assertThat(userid.length()).isEqualTo(8);
        assertThat(userid.initialCursor()).isTrue();
        assertThat(map.field("PASSWD").orElseThrow().dark()).isTrue();
        assertThat(map.field("ERRMSG").orElseThrow().length()).isEqualTo(78);
        assertThat(map.field("ERRMSG").orElseThrow().color()).isEqualTo("RED");
        assertThat(map.fields()).anyMatch(f -> "ENTER=Sign-on  F3=Exit".equals(f.initial()));
    }

    @Test
    void parsesQuotesAndJustify() {
        BmsMapsetDefinition set = BmsParser.parse(SAMPLE);
        BmsMapDefinition map = set.map("TSTMAP");
        assertThat(map.fields()).hasSize(5);
        assertThat(map.fields().getFirst().initial()).isEqualTo("Tran:");
        assertThat(map.field("MSG").orElseThrow().initial()).isEqualTo("It's ok");
        assertThat(map.field("AMT").orElseThrow().justifyRight()).isTrue();
        assertThatThrownBy(() -> set.map("NOPE")).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void receiveMapPadsTruncatesAndFlagsClearedFields() {
        SymbolicMap m = new SymbolicMap(BmsParser.parse(SAMPLE).map("TSTMAP"));
        assertThat(PicX.isLowValues(m.data("NAME"))).isTrue();
        m.receive(Map.of("NAME", "abcdefgh", "SECRET", "", "AMT", "12"));
        assertThat(m.data("NAME")).isEqualTo("abcdef");
        assertThat(m.length("NAME")).isEqualTo(6);
        assertThat(m.length("SECRET")).isZero();
        assertThat(m.flag("SECRET")).isEqualTo(SymbolicMap.FLAG_CLEARED);
        assertThat(PicX.isLowValues(m.data("SECRET"))).isTrue();
        assertThat(m.data("AMT")).isEqualTo("   12");
        assertThat(PicX.isLowValues(m.data("MSG"))).isTrue();
        assertThatThrownBy(() -> m.receive(Map.of("BOGUS", "x"))).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void sendMapUsesInitialValuesForLowValueFieldsAndPositionsCursor() {
        SymbolicMap m = new SymbolicMap(BmsParser.parse(SAMPLE).map("TSTMAP"));
        BmsScreen s = m.render(true, true, false, false);
        assertThat(s.value("MSG")).isEqualTo("It's ok   ");
        assertThat(s.value("NAME")).isEqualTo("      ");
        assertThat(s.cursorField()).isEqualTo("NAME");
        assertThat(s.cursorRow()).isEqualTo(2);
        assertThat(s.cursorColumn()).isEqualTo(11);

        m.setData("MSG", "Error!");
        m.setData("SECRET", "pw");
        m.setCursor("AMT");
        s = m.render(true, true, false, false);
        assertThat(s.value("MSG")).isEqualTo("Error!    ");
        assertThat(s.field("MSG").orElseThrow().sent()).isTrue();
        assertThat(s.cursorField()).isEqualTo("AMT");
        assertThat(s.render().get(23)).startsWith(" Error!");
        assertThat(s.render().get(2)).doesNotContain("pw");
        assertThat(s.render().getFirst()).startsWith(" Tran:");

        BmsScreen dataOnly = m.render(false, false, false, true);
        assertThat(dataOnly.value("NAME")).isNull();
        assertThat(dataOnly.cursorField()).isEqualTo("NAME");

        m.setDataPrefix("NAME", "XY");
        assertThat(m.data("NAME")).isEqualTo("XY\u0000\u0000\u0000\u0000");
        assertThat(m.render(false, false, false, false).value("NAME")).isEqualTo("XY    ");
        m.lowValues();
        assertThat(PicX.isLowValues(m.data("MSG"))).isTrue();
        assertThat(m.length("AMT")).isZero();
    }
}
