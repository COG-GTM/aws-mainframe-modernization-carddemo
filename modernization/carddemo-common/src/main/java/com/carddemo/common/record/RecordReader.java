package com.carddemo.common.record;

import com.carddemo.common.cobol.CodePage;
import com.carddemo.common.error.FileStatus;
import com.carddemo.common.error.FileStatusException;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Optional;

/**
 * Sequential reader of fixed-length records ({@code READ ... AT END}). End of file is the only
 * normal non-record outcome; a record longer than the layout, a partial fixed block or an I/O error
 * is a {@link FileStatusException} (abend), never a skipped record.
 */
public final class RecordReader implements Closeable {

    private final RecordLayout layout;
    private final CodePage codePage;
    private final RecordFormat format;
    private final String ddName;
    private final InputStream in;
    private long recordsRead;

    private RecordReader(RecordLayout layout, CodePage codePage, RecordFormat format, String ddName, InputStream in) {
        this.layout = layout;
        this.codePage = codePage;
        this.format = format;
        this.ddName = ddName;
        this.in = in;
    }

    public static RecordReader open(Path file, String ddName, RecordLayout layout, CodePage codePage, RecordFormat format) {
        try {
            return new RecordReader(layout, codePage, format, ddName, new BufferedInputStream(Files.newInputStream(file)));
        } catch (NoSuchFileException ex) {
            throw FileStatusException.of(FileStatus.FILE_NOT_FOUND, ddName, "OPEN", ex);
        } catch (IOException ex) {
            throw FileStatusException.of(FileStatus.PERMANENT_ERROR, ddName, "OPEN", ex);
        }
    }

    public static RecordReader of(InputStream in, String ddName, RecordLayout layout, CodePage codePage, RecordFormat format) {
        return new RecordReader(layout, codePage, format, ddName, in.markSupported() ? in : new BufferedInputStream(in));
    }

    /** The next record, or empty at end of file (FILE STATUS 10). */
    public Optional<CobolRecord> read() {
        try {
            byte[] image = format == RecordFormat.FIXED_BLOCK ? readFixed() : readLine();
            if (image == null) {
                return Optional.empty();
            }
            recordsRead++;
            return Optional.of(new CobolRecord(layout, codePage, image));
        } catch (IOException ex) {
            throw FileStatusException.of(FileStatus.PERMANENT_ERROR, ddName, "READ", ex);
        }
    }

    public long recordsRead() {
        return recordsRead;
    }

    private byte[] readFixed() throws IOException {
        byte[] image = in.readNBytes(layout.length());
        if (image.length == 0) {
            return null;
        }
        if (image.length != layout.length()) {
            throw lengthConflict("partial record of " + image.length + " bytes at end of file");
        }
        return image;
    }

    private byte[] readLine() throws IOException {
        ByteArrayOutputStream line = new ByteArrayOutputStream(layout.length());
        int b = in.read();
        if (b < 0) {
            return null;
        }
        while (b >= 0 && b != '\n') {
            line.write(b);
            b = in.read();
        }
        byte[] text = line.toByteArray();
        int len = text.length;
        if (len > 0 && text[len - 1] == '\r') {
            len--;
        }
        if (len > layout.length()) {
            throw lengthConflict("line of " + len + " bytes");
        }
        byte[] image = new byte[layout.length()];
        System.arraycopy(text, 0, image, 0, len);
        Arrays.fill(image, len, image.length, codePage.space());
        return image;
    }

    private FileStatusException lengthConflict(String detail) {
        return new FileStatusException(FileStatus.RECORD_LENGTH_CONFLICT, ddName,
                "READ record " + (recordsRead + 1) + " (" + detail + ", " + layout.copybook() + " is " + layout.length() + ")");
    }

    @Override
    public void close() {
        try {
            in.close();
        } catch (IOException ex) {
            throw FileStatusException.of(FileStatus.PERMANENT_ERROR, ddName, "CLOSE", ex);
        }
    }
}
