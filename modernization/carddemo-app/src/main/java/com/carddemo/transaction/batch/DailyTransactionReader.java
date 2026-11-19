package com.carddemo.transaction.batch;

import com.carddemo.common.cobol.CodePage;
import com.carddemo.common.record.RecordFormat;
import com.carddemo.common.record.RecordReader;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.batch.item.ItemStreamReader;

import java.nio.file.Path;

/**
 * DD DALYTRAN reader (BND-002). ASCII input is line-sequential (CR stripped, short lines padded to 350, as the
 * oracle normalizes the sample); CP037 input is RECFM=FB. Its read count is saved in the step {@link ExecutionContext}
 * so a restart of the same JobInstance skips the records already committed. A FILE STATUS other than 00/10 abends.
 */
public class DailyTransactionReader implements ItemStreamReader<DailyTransaction> {

    public static final String DD = "DALYTRAN";
    static final String READ_COUNT = "posttran.dalytran.read.count";

    private final Path file;
    private final CodePage codePage;
    private RecordReader reader;
    private long readCount;

    public DailyTransactionReader(Path file, CodePage codePage) {
        this.file = file;
        this.codePage = codePage;
    }

    public static RecordFormat formatFor(CodePage codePage) {
        return codePage == CodePage.ASCII ? RecordFormat.LINE_SEQUENTIAL : RecordFormat.FIXED_BLOCK;
    }

    @Override
    public void open(ExecutionContext context) {
        reader = RecordReader.open(file, DD, DailyTransactionLayout.LAYOUT, codePage, formatFor(codePage));
        long committed = context.getLong(READ_COUNT, 0L);
        for (readCount = 0; readCount < committed; readCount++) {
            if (reader.read().isEmpty()) {
                throw new ItemStreamException(DD + " has " + readCount + " records, restart expects at least " + committed);
            }
        }
    }

    @Override
    public DailyTransaction read() {
        return reader.read().map(r -> new DailyTransaction(++readCount, r)).orElse(null);
    }

    @Override
    public void update(ExecutionContext context) {
        context.putLong(READ_COUNT, readCount);
    }

    @Override
    public void close() {
        if (reader != null) {
            reader.close();
            reader = null;
        }
    }
}
