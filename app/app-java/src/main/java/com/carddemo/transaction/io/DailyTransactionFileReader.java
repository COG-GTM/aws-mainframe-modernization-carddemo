package com.carddemo.transaction.io;

import com.carddemo.transaction.cobol.CobolFileException;
import com.carddemo.transaction.cobol.FileStatus;
import com.carddemo.transaction.domain.DailyTransaction;
import org.springframework.batch.item.support.AbstractItemCountingItemStreamItemReader;
import org.springframework.core.io.Resource;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

/**
 * DALYTRAN sequential input ({@code ORGANIZATION IS SEQUENTIAL}, RECLN 350).
 * <ul>
 *   <li>{@code 0000-DALYTRAN-OPEN}: missing/unreadable file -> {@link CobolFileException} status 35.</li>
 *   <li>{@code 1000-DALYTRAN-GET-NEXT}: status '00' returns the record, status '10' (EOF) returns
 *       {@code null} which ends the step, anything else -> {@link CobolFileException} (abend).</li>
 *   <li>{@code 9000-DALYTRAN-CLOSE}: {@link #doClose()}.</li>
 * </ul>
 * Restartable: Spring Batch stores the read count and repositions on restart.
 */
public class DailyTransactionFileReader extends AbstractItemCountingItemStreamItemReader<DailyTransaction> {

    public static final String DD_NAME = "DALYTRAN";

    private final Resource resource;
    private BufferedReader reader;
    private long recordNumber;

    public DailyTransactionFileReader(Resource resource) {
        this.resource = resource;
        setName("dalytranReader");
    }

    @Override
    protected void doOpen() {
        if (resource == null || !resource.exists() || !resource.isReadable()) {
            throw new CobolFileException(DD_NAME, FileStatus.FILE_NOT_FOUND,
                    "ERROR OPENING DAILY TRANSACTION FILE " + resource, null);
        }
        try {
            reader = new BufferedReader(new InputStreamReader(resource.getInputStream(), StandardCharsets.US_ASCII));
        } catch (IOException e) {
            throw new CobolFileException(DD_NAME, FileStatus.PERMANENT_IO_ERROR,
                    "ERROR OPENING DAILY TRANSACTION FILE", e);
        }
        recordNumber = 0;
    }

    @Override
    protected DailyTransaction doRead() {
        String line;
        try {
            line = reader.readLine();
        } catch (IOException e) {
            throw new CobolFileException(DD_NAME, FileStatus.PERMANENT_IO_ERROR,
                    "ERROR READING DAILY TRANSACTION FILE", e);
        }
        if (line == null) {
            return null;
        }
        recordNumber++;
        String record = line.endsWith("\r") ? line.substring(0, line.length() - 1) : line;
        if (record.length() > DailyTransaction.RECORD_LENGTH) {
            throw new CobolFileException(DD_NAME, "04",
                    "ERROR READING DAILY TRANSACTION FILE - RECORD " + recordNumber + " LENGTH "
                            + record.length() + " EXCEEDS " + DailyTransaction.RECORD_LENGTH, null);
        }
        return RecordLayouts.dailyTransaction(record);
    }

    @Override
    protected void doClose() throws IOException {
        if (reader != null) {
            reader.close();
            reader = null;
        }
    }
}
