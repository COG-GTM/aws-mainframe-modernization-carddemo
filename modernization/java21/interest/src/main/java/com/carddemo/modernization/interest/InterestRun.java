package com.carddemo.modernization.interest;

import com.carddemo.modernization.records.AccountRecord;
import com.carddemo.modernization.records.TransactionRecord;
import java.util.List;

public record InterestRun(List<TransactionRecord> transactions, List<AccountRecord> accounts, List<String> sysout) {
    public InterestRun {
        transactions = List.copyOf(transactions);
        accounts = List.copyOf(accounts);
        sysout = List.copyOf(sysout);
    }
}
