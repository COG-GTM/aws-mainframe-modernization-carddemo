package com.carddemo.programs.cbact04c.io;

import com.carddemo.programs.cbact04c.copybook.RecordCodec;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;

/**
 * Line-oriented fixed-width record files (one copybook record image per line, ISO-8859-1).
 * Trailing CR is dropped and short lines are space-padded to the record length, which is how the
 * ASCII sample files in app/data/ASCII are stored (trailing FILLER spaces trimmed, some CRLF).
 */
public final class FixedWidth {

    public static final Charset CHARSET = StandardCharsets.ISO_8859_1;

    private FixedWidth() {
    }

    public static String normalise(String line, int length) {
        String l = line.endsWith("\r") ? line.substring(0, line.length() - 1) : line;
        if (l.length() > length) {
            throw new IllegalArgumentException("Record longer than " + length + ": " + l.length());
        }
        return l + " ".repeat(length - l.length());
    }

    public static <T> List<T> readAll(Path path, RecordCodec<T> codec) {
        try {
            List<T> out = new ArrayList<>();
            for (String line : Files.readAllLines(path, CHARSET)) {
                if (line.isEmpty() || line.equals("\r")) {
                    continue;
                }
                out.add(codec.decode(normalise(line, codec.length())));
            }
            return out;
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    public static <T> void writeAll(Path path, RecordCodec<T> codec, List<T> records) {
        StringBuilder sb = new StringBuilder();
        for (T r : records) {
            sb.append(codec.encode(r)).append('\n');
        }
        Path tmp = path.resolveSibling(path.getFileName() + ".tmp");
        try {
            Files.writeString(tmp, sb.toString(), CHARSET);
            Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
