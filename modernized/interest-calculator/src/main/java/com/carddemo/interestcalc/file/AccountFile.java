package com.carddemo.interestcalc.file;

import com.carddemo.interestcalc.copybook.AccountRecord;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * ACCTFILE: {@code ORGANIZATION IS INDEXED, ACCESS MODE IS RANDOM, RECORD KEY IS FD-ACCT-ID},
 * opened {@code I-O} because CBACT04C rewrites it in {@code 1050-UPDATE-ACCOUNT}.
 */
public final class AccountFile {

    private final Map<String, AccountRecord> byAccountId;

    private AccountFile(Map<String, AccountRecord> byAccountId) {
        this.byAccountId = byAccountId;
    }

    public static AccountFile load(Path file) {
        Map<String, AccountRecord> map = new TreeMap<>();
        DatasetFiles.read(file, AccountRecord::parse).forEach(record -> map.put(record.accountId(), record));
        return new AccountFile(map);
    }

    /** {@code READ ACCOUNT-FILE INTO ACCOUNT-RECORD}. */
    public KeyedRead<AccountRecord> read(String accountId) {
        AccountRecord record = byAccountId.get(accountId);
        return record == null ? KeyedRead.notFound() : KeyedRead.found(record);
    }

    /** {@code REWRITE FD-ACCTFILE-REC FROM ACCOUNT-RECORD}. */
    public String rewrite(AccountRecord record) {
        if (!byAccountId.containsKey(record.accountId())) {
            return FileStatus.NOT_FOUND;
        }
        byAccountId.put(record.accountId(), record);
        return FileStatus.OK;
    }

    /** The account master in ascending key order, as a sequential unload would produce it. */
    public List<String> unload() {
        return byAccountId.values().stream().map(AccountRecord::format).toList();
    }
}
