package com.carddemo.batch.jobs.creastmt;

import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.batch.item.ItemStreamWriter;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * STMTFILE (RECFM=FB, LRECL=80) and HTMLFILE (RECFM=FB, LRECL=100): fixed-length records,
 * written back to back with no line terminators, as the COBOL WRITE produces them.
 */
public class StatementFileWriter implements ItemStreamWriter<Statement> {

    private final Path stmtFile;
    private final Path htmlFile;
    private OutputStream stmtOut;
    private OutputStream htmlOut;

    public StatementFileWriter(Path stmtFile, Path htmlFile) {
        this.stmtFile = stmtFile;
        this.htmlFile = htmlFile;
    }

    @Override
    public void open(ExecutionContext executionContext) {
        try {
            stmtOut = open(stmtFile);
            htmlOut = open(htmlFile);
        } catch (IOException e) {
            throw new ItemStreamException("Cannot open STMTFILE/HTMLFILE", e);
        }
    }

    @Override
    public void write(Chunk<? extends Statement> chunk) {
        try {
            for (Statement s : chunk) {
                writeRecords(stmtOut, s.stmtRecords(), StatementFormatter.STMT_LRECL);
                writeRecords(htmlOut, s.htmlRecords(), StatementFormatter.HTML_LRECL);
            }
            stmtOut.flush();
            htmlOut.flush();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public void close() {
        try {
            if (stmtOut != null) {
                stmtOut.close();
            }
            if (htmlOut != null) {
                htmlOut.close();
            }
        } catch (IOException e) {
            throw new ItemStreamException("Cannot close STMTFILE/HTMLFILE", e);
        }
    }

    private static OutputStream open(Path file) throws IOException {
        Path parent = file.toAbsolutePath().getParent();
        if (parent != null) {
            Files.createDirectories(parent);
        }
        return new BufferedOutputStream(Files.newOutputStream(file));
    }

    private static void writeRecords(OutputStream out, Iterable<String> records, int lrecl) throws IOException {
        for (String r : records) {
            if (r.length() != lrecl) {
                throw new IllegalStateException("Record length " + r.length() + " != LRECL " + lrecl);
            }
            out.write(r.getBytes(StandardCharsets.US_ASCII));
        }
    }
}
