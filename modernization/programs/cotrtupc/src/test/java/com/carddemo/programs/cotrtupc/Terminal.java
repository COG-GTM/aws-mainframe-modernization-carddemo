package com.carddemo.programs.cotrtupc;

import com.carddemo.programs.cotrtupc.copybook.CarddemoCommarea;
import com.carddemo.programs.cotrtupc.copybook.CtrtupaMap;
import com.carddemo.programs.cotrtupc.copybook.Dfhaid;
import com.carddemo.programs.cotrtupc.gateway.InMemoryTransactionTypeGateway;

import java.time.Clock;
import java.time.LocalDateTime;
import java.time.ZoneOffset;

/** Drives CTTU pseudo-conversationally: keeps the COMMAREA returned by the last task. */
final class Terminal {

    static final Clock CLOCK = Clock.fixed(LocalDateTime.of(2026, 11, 26, 9, 5, 7).toInstant(ZoneOffset.UTC),
            ZoneOffset.UTC);

    final InMemoryTransactionTypeGateway db;
    final Cotrtupc program;
    String commarea;
    TranTypeMaintResponse last;

    Terminal(InMemoryTransactionTypeGateway db) {
        this.db = db;
        this.program = new Cotrtupc(db, db, CLOCK);
    }

    static Terminal withSeedData() {
        return new Terminal(new InMemoryTransactionTypeGateway()
                .put("01", "PURCHASE").put("02", "PAYMENT").put("03", "CREDIT")
                .put("04", "AUTHORIZATION").put("05", "REFUND").put("06", "REVERAL").put("07", "ADJUSTMENT"));
    }

    TranTypeMaintResponse press(Dfhaid aid, String type, String desc) {
        last = program.execute(new TranTypeMaintRequest(aid, commarea, type, desc));
        if (last.outcome() == TranTypeMaintResponse.Outcome.RETURN) {
            commarea = last.commarea();
        }
        return last;
    }

    TranTypeMaintResponse press(Dfhaid aid) {
        return press(aid, null, null);
    }

    /** Resend what the screen currently shows (protected fields carry MDT, so the 3270 returns them). */
    TranTypeMaintResponse pressResending(Dfhaid aid) {
        CtrtupaMap s = last.screen();
        return press(aid, transmitted(s.trtypcd), transmitted(s.trtydsc));
    }

    private static String transmitted(CtrtupaMap.BmsField f) {
        String d = f.data.replace("\u0000", "");
        return d.isEmpty() ? null : d;
    }

    /** Start the conversation as the admin menu does after selecting the option. */
    Terminal start() {
        press(Dfhaid.DFHENTER);
        return this;
    }

    TranTypeMaintResponse show(String type) {
        return press(Dfhaid.DFHENTER, type, null);
    }

    char state() {
        return commarea.charAt(CarddemoCommarea.LENGTH);
    }

    CarddemoCommarea carddemo() {
        CarddemoCommarea c = new CarddemoCommarea();
        c.fromFixed(commarea.substring(0, CarddemoCommarea.LENGTH));
        return c;
    }

    TtupProgCommarea ttup() {
        TtupProgCommarea p = new TtupProgCommarea();
        p.fromFixed(commarea.substring(CarddemoCommarea.LENGTH, CarddemoCommarea.LENGTH + TtupProgCommarea.LENGTH));
        return p;
    }

    String info() {
        return last.screen().infomsg.displayText().trim();
    }

    String error() {
        return last.screen().errmsg.displayText().trim();
    }

    String errmsgRaw() {
        return last.screen().errmsg.data;
    }
}
