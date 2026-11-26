package com.carddemo.programs.coactvwc.repository;

import com.carddemo.programs.coactvwc.copybook.AccountRecord;

/** Dataset ACCTDAT: account master KSDS keyed by ACCT-ID. */
public interface AccountRepository {
    /** READ DATASET('ACCTDAT') RIDFLD(acctIdX) KEYLENGTH(11). */
    FileReadResult<AccountRecord> read(String acctIdX);
}
