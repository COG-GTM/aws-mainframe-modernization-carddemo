package com.carddemo.transaction.batch;

import com.carddemo.common.record.Copybooks;
import com.carddemo.transaction.domain.DailyTransaction;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.batch.item.support.AbstractItemCountingItemStreamItemReader;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * DALYTRAN reader (boundary B-01): RECFM=FB, 350-byte CVTRA06Y records, ISO-8859-1 or CP037. Restartable: the item
 * count is saved in the step ExecutionContext after each commit and a restart skips that many records (D-012).
 * A trailing partial record fails the step (FR-POSTTRAN-13, D-006).
 */
public class DailyTransactionReader extends AbstractItemCountingItemStreamItemReader<DailyTransaction> {

    static final int RECORD_LENGTH = Copybooks.CVTRA06Y.length();

    private final Path file;
    private final Charset charset;
    private InputStream in;

    public DailyTransactionReader(Path file, Charset charset) {
        this.file = file;
        this.charset = charset;
        setName("dalytran");
    }

    @Override
    protected void doOpen() throws IOException {
        in = new BufferedInputStream(Files.newInputStream(file));
    }

    @Override
    protected DailyTransaction doRead() throws IOException {
        byte[] buffer = in.readNBytes(RECORD_LENGTH);
        if (buffer.length == 0) {
            return null;
        }
        if (buffer.length < RECORD_LENGTH) {
            throw new ItemStreamException(file + ": short DALYTRAN record " + (getCurrentItemCount()) + " ("
                    + buffer.length + " of " + RECORD_LENGTH + " bytes)");
        }
        return new DailyTransaction(new String(buffer, charset));
    }

    @Override
    protected void jumpToItem(int itemIndex) throws IOException {
        try {
            in.skipNBytes((long) itemIndex * RECORD_LENGTH);
        } catch (java.io.EOFException e) {
            throw new ItemStreamException(file + " has fewer than " + itemIndex + " records on restart", e);
        }
    }

    @Override
    protected void doClose() throws IOException {
        if (in != null) {
            in.close();
            in = null;
        }
    }
}
