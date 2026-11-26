package com.carddemo.programs.cousr00c;

import com.carddemo.programs.cousr00c.copybook.CardDemoCommarea;
import com.carddemo.programs.cousr00c.copybook.Dfhaid;
import com.carddemo.programs.cousr00c.copybook.SecUserData;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;

final class Fixtures {

    static final Clock CLOCK = Clock.fixed(LocalDateTime.of(2026, 11, 26, 17, 22, 5, 370_000_000)
            .toInstant(ZoneOffset.UTC), ZoneOffset.UTC);

    private Fixtures() {
    }

    static List<SecUserData> users(int n) {
        List<SecUserData> out = new ArrayList<>();
        for (int i = 1; i <= n; i++) {
            String id = String.format("USER%04d", i);
            out.add(SecUserData.of(id, "First" + i, "Last" + i, "pw", i % 2 == 0 ? "A" : "U"));
        }
        return out;
    }

    static String userId(int i) {
        return String.format("USER%04d", i);
    }

    /** CARDDEMO-COMMAREA as COADM01C passes it on XCTL (160 bytes, context ENTER). */
    static String adminMenuCommarea() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.setFromTranid("CA00");
        c.setFromProgram("COADM01C");
        c.setUserId("ADMIN001");
        c.setUserType(CardDemoCommarea.UserType.ADMIN);
        c.setPgmContext(CardDemoCommarea.PgmContext.ENTER);
        return c.toFixedWidth();
    }

    static Cousr00cRequest firstEntry() {
        return new Cousr00cRequest(Dfhaid.DFHENTER, adminMenuCommarea(), null);
    }

    /** Next task after {@code previous}: the terminal echoes the screen, optionally edited. */
    static Cousr00cRequest next(Cousr00cResponse previous, Dfhaid aid, Cousr0aInput input) {
        return new Cousr00cRequest(aid, previous.commarea().toFixedWidth(), input);
    }

    static Cousr00cRequest next(Cousr00cResponse previous, Dfhaid aid) {
        return next(previous, aid, Cousr0aInput.echo(previous.screen()));
    }

    static String trim(String s) {
        return s.replace('\u0000', ' ').stripTrailing();
    }
}
