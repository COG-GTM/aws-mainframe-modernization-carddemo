package com.carddemo.batch.harness;

import com.carddemo.common.AbendException;
import com.carddemo.common.file.FileStatusException;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * {@code //SYSOUT DD SYSOUT=*}: the lines a program {@code DISPLAY}s, one per line, ISO-8859-1 so each character of
 * a record image is one byte (as GnuCOBOL writes the record bytes of an ASCII image).
 */
public final class Sysout implements AutoCloseable {

    private static final Logger log = LoggerFactory.getLogger(Sysout.class);

    private final Path path;
    private final BufferedWriter out;
    private long lines;

    private Sysout(Path path, BufferedWriter out) {
        this.path = path;
        this.out = out;
    }

    public static Sysout open(Path path) {
        try {
            Path parent = path.toAbsolutePath().getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            return new Sysout(path, Files.newBufferedWriter(path, StandardCharsets.ISO_8859_1));
        } catch (IOException e) {
            throw new UncheckedIOException("cannot open SYSOUT " + path, e);
        }
    }

    /** {@code DISPLAY a b c}: the items concatenated on one line. */
    public void display(String... items) {
        String line = String.join("", items);
        try {
            out.write(line);
            out.write('\n');
        } catch (IOException e) {
            throw new UncheckedIOException("cannot write SYSOUT " + path, e);
        }
        lines++;
        log.debug("SYSOUT {}", line);
    }

    /**
     * The programs' error path: {@code DISPLAY message}, {@code 9910-DISPLAY-IO-STATUS},
     * {@code 9999-ABEND-PROGRAM} ({@code ABENDING PROGRAM} + {@code CEE3ABD} 999). Returns the abend to throw.
     * Some programs name the same two paragraphs {@code Z-DISPLAY-IO-STATUS} and {@code Z-ABEND-PROGRAM}.
     */
    public AbendException ioAbend(String message, FileStatusException failure) {
        display(message);
        display(failure.status().displayIoStatus());
        display("ABENDING PROGRAM");
        return AbendException.carddemo(message, failure);
    }

    public long lines() {
        return lines;
    }

    public Path path() {
        return path;
    }

    @Override
    public void close() {
        try {
            out.close();
        } catch (IOException e) {
            throw new UncheckedIOException("cannot close SYSOUT " + path, e);
        }
    }
}
