package com.carddemo.runtime.vsam;

import com.carddemo.runtime.cobol.PicX;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/** Reads seed data: line-sequential text (each line padded/truncated to LRECL) or fixed-length binary. */
public final class FlatFiles {

    private FlatFiles() {
    }

    public static List<String> readLines(InputStream in, int lrecl) {
        try {
            String text = new String(in.readAllBytes(), PicX.CHARSET);
            List<String> out = new ArrayList<>();
            for (String line : text.split("\r?\n")) {
                if (!line.isEmpty()) {
                    out.add(PicX.of(line, lrecl));
                }
            }
            return out;
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    public static List<String> readLines(Path path, int lrecl) {
        try (InputStream in = Files.newInputStream(path)) {
            return readLines(in, lrecl);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /** Fixed-length records (RECFM=F/FB) with no delimiters. */
    public static List<String> readFixed(byte[] data, int lrecl) {
        List<String> out = new ArrayList<>();
        for (int i = 0; i + lrecl <= data.length; i += lrecl) {
            out.add(new String(data, i, lrecl, PicX.CHARSET));
        }
        return out;
    }
}
