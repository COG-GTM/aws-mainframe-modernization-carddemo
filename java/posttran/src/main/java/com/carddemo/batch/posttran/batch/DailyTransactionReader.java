package com.carddemo.batch.posttran.batch;

import com.carddemo.batch.posttran.cobol.Abend;
import com.carddemo.batch.posttran.cobol.Pic;
import com.carddemo.batch.posttran.cobol.RecordFormat;
import com.carddemo.batch.posttran.record.DailyTransaction;
import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import org.springframework.batch.infrastructure.item.support.AbstractItemCountingItemStreamItemReader;

/** DALYTRAN DD: 0000-DALYTRAN-OPEN, 1000-DALYTRAN-GET-NEXT, 9000-DALYTRAN-CLOSE. */
public class DailyTransactionReader extends AbstractItemCountingItemStreamItemReader<DailyTransaction> {

    private final Path path;
    private final RecordFormat format;
    private InputStream in;

    public DailyTransactionReader(Path path, RecordFormat format) {
        this.path = path;
        this.format = format;
        setName("dalytran");
        setSaveState(false);
    }

    @Override
    protected void doOpen() {
        try {
            in = new BufferedInputStream(Files.newInputStream(path));
        } catch (IOException e) {
            throw new Abend("ERROR OPENING DALYTRAN", "35", e);
        }
    }

    @Override
    protected DailyTransaction doRead() {
        String record;
        try {
            record = format.lineDelimited() ? readLine() : readFixed();
        } catch (IOException e) {
            throw new Abend("ERROR READING DALYTRAN FILE", "30", e);
        }
        if (record == null) {
            return null;
        }
        try {
            return DailyTransaction.parse(record);
        } catch (RuntimeException e) {
            throw new Abend("ERROR READING DALYTRAN FILE", "04", e);
        }
    }

    private String readLine() throws IOException {
        ByteArrayOutputStream buf = new ByteArrayOutputStream(DailyTransaction.LENGTH + 2);
        int b = in.read();
        if (b < 0) {
            return null;
        }
        while (b >= 0 && b != '\n') {
            if (b != '\r') {
                buf.write(b);
            }
            b = in.read();
        }
        String line = buf.toString(format.charset());
        if (line.length() > DailyTransaction.LENGTH) {
            throw new Abend("ERROR READING DALYTRAN FILE", "04");
        }
        return Pic.x(line, DailyTransaction.LENGTH);
    }

    private String readFixed() throws IOException {
        byte[] bytes = in.readNBytes(DailyTransaction.LENGTH);
        if (bytes.length == 0) {
            return null;
        }
        if (bytes.length != DailyTransaction.LENGTH) {
            throw new Abend("ERROR READING DALYTRAN FILE", "04");
        }
        return new String(bytes, format.charset());
    }

    @Override
    protected void doClose() throws IOException {
        if (in != null) {
            in.close();
            in = null;
        }
    }
}
