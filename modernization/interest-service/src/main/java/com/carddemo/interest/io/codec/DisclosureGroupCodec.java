package com.carddemo.interest.io.codec;

import com.carddemo.interest.domain.DisclosureGroup;
import com.carddemo.interest.domain.DisclosureGroupKey;
import com.carddemo.interest.domain.TransactionCategory;
import com.carddemo.mainframe.io.RecordLayout;
import com.carddemo.mainframe.io.layout.CardDemoLayouts;

/** Maps {@code CVTRA02Y} records to {@link DisclosureGroup} instances. */
public final class DisclosureGroupCodec {

    private static final RecordLayout LAYOUT = CardDemoLayouts.DISCLOSURE_GROUP;

    private DisclosureGroupCodec() {
    }

    public static int recordLength() {
        return LAYOUT.recordLength();
    }

    public static DisclosureGroup decode(byte[] record) {
        TransactionCategory category = TransactionCategory.of(
                LAYOUT.rawText(record, "transactionTypeCode"),
                LAYOUT.decimal(record, "transactionCategoryCode").intValueExact());
        return new DisclosureGroup(
                DisclosureGroupKey.of(LAYOUT.text(record, "accountGroupId"), category),
                LAYOUT.decimal(record, "interestRate"));
    }
}
