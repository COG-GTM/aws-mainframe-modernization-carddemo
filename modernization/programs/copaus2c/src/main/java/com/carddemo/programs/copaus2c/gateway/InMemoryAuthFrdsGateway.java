package com.carddemo.programs.copaus2c.gateway;

import com.carddemo.programs.copaus2c.copybook.AuthFrdsRow;
import com.carddemo.programs.copaus2c.copybook.Sqlca;

import java.time.Clock;
import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

/**
 * In-memory CARDDEMO.AUTHFRDS with DB2 outcomes: PRIMARY KEY(CARD_NUM, AUTH_TS) violations return
 * SQLCODE -803 / SQLSTATE 23505, an UPDATE touching no row returns +100 / 02000, and an invalid
 * TIMESTAMP_FORMAT input returns -20448 / 22007. Failures can be scripted with {@link #failNextInsert}
 * and {@link #failNextUpdate}.
 */
public final class InMemoryAuthFrdsGateway implements AuthFrdsGateway {

    public static final Sqlca DUPLICATE = new Sqlca(Sqlca.DUPLICATE_KEY, "23505");
    public static final Sqlca NOT_FOUND = new Sqlca(Sqlca.NOT_FOUND, "02000");
    public static final Sqlca INVALID_TIMESTAMP = new Sqlca(-20448, "22007");

    /** A persisted AUTHFRDS row: the bound host variables plus the DB2-computed AUTH_TS and FRAUD_RPT_DATE. */
    public record StoredRow(AuthFrdsRow row, LocalDateTime authTs, String authFraud, LocalDate fraudRptDate) {
    }

    private record Key(String cardNum, LocalDateTime authTs) {
    }

    private final Clock clock;
    private final Map<Key, StoredRow> table = new LinkedHashMap<>();
    private final Deque<Sqlca> insertFailures = new ArrayDeque<>();
    private final Deque<Sqlca> updateFailures = new ArrayDeque<>();

    public InMemoryAuthFrdsGateway(Clock clock) {
        this.clock = clock;
    }

    public void failNextInsert(Sqlca sqlca) {
        insertFailures.add(sqlca);
    }

    public void failNextUpdate(Sqlca sqlca) {
        updateFailures.add(sqlca);
    }

    public Optional<StoredRow> find(String cardNum, LocalDateTime authTs) {
        return Optional.ofNullable(table.get(new Key(cardNum, authTs)));
    }

    public Collection<StoredRow> rows() {
        return Collections.unmodifiableCollection(table.values());
    }

    @Override
    public Sqlca insert(AuthFrdsRow row) {
        if (!insertFailures.isEmpty()) {
            return insertFailures.poll();
        }
        LocalDate today = LocalDate.now(clock);
        LocalDateTime ts;
        try {
            ts = Db2TimestampFormat.parse(row.authTs(), today);
        } catch (DateTimeException e) {
            return INVALID_TIMESTAMP;
        }
        Key key = new Key(row.cardNum(), ts);
        if (table.containsKey(key)) {
            return DUPLICATE;
        }
        table.put(key, new StoredRow(row, ts, row.authFraud(), today));
        return Sqlca.OK;
    }

    @Override
    public Sqlca updateFraud(String authFraud, String cardNum, String authTs) {
        if (!updateFailures.isEmpty()) {
            return updateFailures.poll();
        }
        LocalDate today = LocalDate.now(clock);
        LocalDateTime ts;
        try {
            ts = Db2TimestampFormat.parse(authTs, today);
        } catch (DateTimeException e) {
            return INVALID_TIMESTAMP;
        }
        Key key = new Key(cardNum, ts);
        StoredRow existing = table.get(key);
        if (existing == null) {
            return NOT_FOUND;
        }
        table.put(key, new StoredRow(existing.row(), ts, authFraud, today));
        return Sqlca.OK;
    }
}
