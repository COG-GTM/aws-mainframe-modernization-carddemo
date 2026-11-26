package com.carddemo.programs.cbcus01c;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

/**
 * File-backed CUSTFILE: a text file of fixed-width 500-byte customer records (one per line, as in
 * app/data/ASCII/custdata.txt), presented in ascending FD-CUST-ID order like a KSDS read with
 * ACCESS MODE SEQUENTIAL. Short lines are space padded and long lines truncated to 500 bytes.
 */
public final class KsdsCustomerFile implements CustomerFile {

    private final Path path;
    private final Charset charset;
    private Iterator<FdCustfileRec> cursor;

    public KsdsCustomerFile(Path path) {
        this(path, StandardCharsets.US_ASCII);
    }

    public KsdsCustomerFile(Path path, Charset charset) {
        this.path = path;
        this.charset = charset;
    }

    @Override
    public String open() {
        if (cursor != null) {
            return "41";
        }
        try {
            List<FdCustfileRec> records = new ArrayList<>();
            for (String line : Files.readAllLines(path, charset)) {
                if (!line.isEmpty()) {
                    records.add(FdCustfileRec.fromFixedWidth(line));
                }
            }
            records.sort(Comparator.comparing(FdCustfileRec::fdCustId));
            cursor = records.iterator();
            return FileStatus.SUCCESS;
        } catch (NoSuchFileException e) {
            return FileStatus.FILE_NOT_FOUND;
        } catch (IOException e) {
            return "30";
        }
    }

    @Override
    public ReadResult readNext() {
        if (cursor == null) {
            return ReadResult.status(FileStatus.LOGIC_ERROR_READ_NOT_OPEN);
        }
        return cursor.hasNext() ? ReadResult.ok(cursor.next()) : ReadResult.status(FileStatus.END_OF_FILE);
    }

    @Override
    public String close() {
        if (cursor == null) {
            return FileStatus.NOT_OPEN;
        }
        cursor = null;
        return FileStatus.SUCCESS;
    }
}
