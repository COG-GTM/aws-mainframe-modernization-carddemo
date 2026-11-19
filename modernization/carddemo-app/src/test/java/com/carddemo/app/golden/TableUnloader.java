package com.carddemo.app.golden;

import com.carddemo.account.batch.AccountRecordLayout;
import com.carddemo.account.repository.AccountRepository;
import com.carddemo.common.cobol.CodePage;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.reference.batch.TranCatBalanceRecordLayout;
import com.carddemo.reference.repository.TranCatBalanceRepository;
import org.springframework.data.domain.Sort;

import java.util.Comparator;
import java.util.List;

/**
 * Unloads tables to text exactly like the oracle's ULACCT/ULTCAT: one ASCII record image per line in key order,
 * trailing spaces dropped (GnuCOBOL LINE SEQUENTIAL WRITE). TRANSACT (ULTRAN) waits for V7 / OQ-7.
 */
public final class TableUnloader {

    private TableUnloader() {
    }

    public static List<String> accounts(AccountRepository repository) {
        return repository.findAll(Sort.by("acctId")).stream()
                .map(a -> line(AccountRecordLayout.toRecord(a, CodePage.ASCII))).toList();
    }

    public static List<String> tranCatBalances(TranCatBalanceRepository repository) {
        // TRAN-CAT-KEY order = the record image's first 17 bytes (acct 9(11), type X(02), cat 9(04))
        return repository.findAll().stream()
                .map(t -> line(TranCatBalanceRecordLayout.toRecord(t, CodePage.ASCII)))
                .sorted(Comparator.comparing(l -> l.substring(0, 17))).toList();
    }

    static String line(CobolRecord record) {
        return CodePage.ASCII.decode(record.bytes()).stripTrailing();
    }
}
