package com.carddemo.modernization.batch;

import com.carddemo.modernization.interest.InterestInput;
import com.carddemo.modernization.records.AccountRecord;
import com.carddemo.modernization.records.CardXrefRecord;
import com.carddemo.modernization.records.DisclosureGroupRecord;
import com.carddemo.modernization.records.TranCatBalanceRecord;
import com.carddemo.modernization.records.codec.RecordCodec;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.util.List;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamReader;

/**
 * CBACT04C reads its four input datasets as one unit: {@link com.carddemo.modernization.interest.InterestCalculator}
 * needs every category balance, account, cross-reference and disclosure group at once, so the step is one chunk.
 */
final class InterestInputReader implements ItemStreamReader<InterestInput> {
    private final CycleProperties.Interest dd;
    private final Charset charset;
    private boolean consumed;

    InterestInputReader(CycleProperties.Interest dd, Charset charset) {
        this.dd = dd;
        this.charset = charset;
    }

    @Override
    public void open(ExecutionContext context) {
        consumed = false;
    }

    @Override
    public InterestInput read() {
        if (consumed) {
            return null;
        }
        consumed = true;
        return new InterestInput(decode(dd.tcatbalf(), TranCatBalanceRecord.CODEC),
                decode(dd.acctfile(), AccountRecord.CODEC), decode(dd.xreffile(), CardXrefRecord.CODEC),
                decode(dd.discgrp(), DisclosureGroupRecord.CODEC));
    }

    private <T> List<T> decode(Path file, RecordCodec<T> codec) {
        return FixedLengthFiles.read(file, codec.length()).stream().map(r -> codec.decode(r, charset)).toList();
    }
}
