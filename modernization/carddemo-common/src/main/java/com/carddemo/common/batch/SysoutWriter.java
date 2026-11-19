package com.carddemo.common.batch;

import java.io.BufferedWriter;
import java.io.Closeable;
import java.io.IOException;
import java.io.PrintStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * COBOL {@code DISPLAY} replacement: each call writes one line, the items concatenated without
 * separators as DISPLAY does, to the generation's {@code SYSOUT} file and to a console stream. Text
 * is written verbatim (count lines and messages must match the oracle byte for byte); card numbers
 * are only ever passed as {@link Pan}, which renders masked to the last 4 digits. Every line is
 * flushed so SYSOUT is complete up to an abend.
 */
public final class SysoutWriter implements Closeable {

    private final BufferedWriter file;
    private final PrintStream console;

    private SysoutWriter(BufferedWriter file, PrintStream console) {
        this.file = file;
        this.console = console;
    }

    public static SysoutWriter open(Path sysout, PrintStream console) {
        try {
            return new SysoutWriter(Files.newBufferedWriter(sysout, StandardCharsets.ISO_8859_1), console);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    public static SysoutWriter open(GenerationDirectory generation, PrintStream console) {
        return open(generation.resolve(GenerationDirectory.SYSOUT), console);
    }

    public void display(Object... items) {
        StringBuilder line = new StringBuilder();
        for (Object item : items) {
            line.append(item);
        }
        String text = line.toString();
        try {
            file.write(text);
            file.write('\n');
            file.flush();
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
        if (console != null) {
            console.println(text);
        }
    }

    @Override
    public void close() {
        try {
            file.close();
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }
}
