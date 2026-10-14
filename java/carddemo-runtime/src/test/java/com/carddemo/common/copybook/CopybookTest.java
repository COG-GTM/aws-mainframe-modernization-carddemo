package com.carddemo.common.copybook;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.runtime.cobol.PicX;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.Test;

class CopybookTest {

    @Test
    void commareaLayoutMatchesCocom01y() {
        // COCOM01Y lines 19-44: 34 (general) + 84 (customer) + 12 (account) + 16 (card) + 14 (more) = 160
        assertThat(CardDemoCommarea.LENGTH).isEqualTo(160);
        assertThat(CardDemoCommarea.USER_ID.offset()).isEqualTo(24);
        assertThat(CardDemoCommarea.PGM_CONTEXT.offset()).isEqualTo(33);
        assertThat(CardDemoCommarea.CUST_ID.offset()).isEqualTo(34);
        assertThat(CardDemoCommarea.ACCT_ID.offset()).isEqualTo(118);
        assertThat(CardDemoCommarea.CARD_NUM.offset()).isEqualTo(130);
        assertThat(CardDemoCommarea.LAST_MAPSET.offset()).isEqualTo(153);

        CardDemoCommarea c = new CardDemoCommarea();
        assertThat(c.isAdmin()).isFalse();
        assertThat(c.isUser()).isFalse();
        c.setUserType("A");
        c.setPgmContext(0);
        c.setFromProgram("COSGN00C");
        assertThat(c.isAdmin()).isTrue();
        assertThat(c.isPgmEnter()).isTrue();
        assertThat(c.pgmContextRaw()).isEqualTo("0");
        assertThat(CardDemoCommarea.of(c.toBytes()).fromProgram()).isEqualTo("COSGN00C");
        assertThat(CardDemoCommarea.of(new byte[] {'C'}).fromTranid()).isEqualTo("C   ");
    }

    @Test
    void userSecurityRecordMatchesCsusr01y() {
        assertThat(UserSecurityRecord.LENGTH).isEqualTo(80);
        assertThat(UserSecurityRecord.USR_PWD.offset()).isEqualTo(48);
        assertThat(UserSecurityRecord.USR_TYPE.offset()).isEqualTo(56);
        UserSecurityRecord r = UserSecurityRecord.of("ADMIN001MARGARET            GOLD                PASSWORDA");
        assertThat(r.userId()).isEqualTo("ADMIN001");
        assertThat(r.firstName()).isEqualTo(PicX.of("MARGARET", 20));
        assertThat(r.password()).isEqualTo("PASSWORD");
        assertThat(r.userType()).isEqualTo("A");
    }

    @Test
    void seedMatchesDusrsecjInstreamData() throws IOException {
        List<String> seed = UserSecuritySeed.records();
        assertThat(seed).hasSize(10).allMatch(s -> s.length() == 80);
        Path jcl = Path.of(System.getProperty("carddemo.app.dir", "../../app"), "jcl", "DUSRSECJ.jcl");
        List<String> instream = Files.readAllLines(jcl, StandardCharsets.ISO_8859_1).stream()
                .filter(l -> l.startsWith("ADMIN0") || l.startsWith("USER00")).map(l -> PicX.of(l, 80)).toList();
        assertThat(seed).containsExactlyElementsOf(instream);
    }

    @Test
    void constantsAndDateFields() {
        assertThat(ScreenTitles.CCDA_TITLE01).hasSize(40).contains("AWS Mainframe Modernization");
        assertThat(CommonMessages.CCDA_MSG_THANK_YOU).hasSize(50).startsWith("Thank you for using CardDemo application...");
        assertThat(CommonMessages.CCDA_MSG_INVALID_KEY).startsWith("Invalid key pressed. Please see below...");
        DateTimeFields dt = DateTimeFields.fromCurrentDate("2024030502080912-0500");
        assertThat(dt.curdateMmDdYy()).isEqualTo("03/05/24");
        assertThat(dt.curtimeHhMmSs()).isEqualTo("02:08:09");
        assertThat(dt.curtimeMilsec()).isEqualTo("12");
    }
}
