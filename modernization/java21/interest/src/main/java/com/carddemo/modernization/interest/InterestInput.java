package com.carddemo.modernization.interest;

import com.carddemo.modernization.records.AccountRecord;
import com.carddemo.modernization.records.CardXrefRecord;
import com.carddemo.modernization.records.DisclosureGroupRecord;
import com.carddemo.modernization.records.TranCatBalanceRecord;
import java.util.List;

public record InterestInput(
        List<TranCatBalanceRecord> categoryBalances,
        List<AccountRecord> accounts,
        List<CardXrefRecord> crossReferences,
        List<DisclosureGroupRecord> disclosureGroups) {}
