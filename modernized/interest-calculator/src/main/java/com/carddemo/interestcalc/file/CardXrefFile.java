package com.carddemo.interestcalc.file;

import com.carddemo.interestcalc.copybook.CardXrefRecord;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

/**
 * XREFFILE: {@code ORGANIZATION IS INDEXED, ACCESS MODE IS RANDOM, RECORD KEY IS
 * FD-XREF-CARD-NUM, ALTERNATE RECORD KEY IS FD-XREF-ACCT-ID}. CBACT04C only ever reads it
 * through the alternate key ({@code READ ... KEY IS FD-XREF-ACCT-ID}).
 *
 * <p>The alternate key is declared without {@code WITH DUPLICATES}, so an account maps to at
 * most one card; loading rejects datasets that violate that.
 */
public final class CardXrefFile {

    private final Map<String, CardXrefRecord> byAccountId;

    private CardXrefFile(Map<String, CardXrefRecord> byAccountId) {
        this.byAccountId = byAccountId;
    }

    public static CardXrefFile load(Path file) {
        Map<String, CardXrefRecord> map = new HashMap<>();
        for (CardXrefRecord record : DatasetFiles.read(file, CardXrefRecord::parse)) {
            CardXrefRecord previous = map.put(record.accountId(), record);
            if (previous != null) {
                throw new IllegalStateException(
                        "duplicate alternate key XREF-ACCT-ID " + record.accountId()
                                + "; the copybook declares it without WITH DUPLICATES");
            }
        }
        return new CardXrefFile(map);
    }

    /** {@code READ XREF-FILE INTO CARD-XREF-RECORD KEY IS FD-XREF-ACCT-ID}. */
    public KeyedRead<CardXrefRecord> readByAccountId(String accountId) {
        CardXrefRecord record = byAccountId.get(accountId);
        return record == null ? KeyedRead.notFound() : KeyedRead.found(record);
    }
}
