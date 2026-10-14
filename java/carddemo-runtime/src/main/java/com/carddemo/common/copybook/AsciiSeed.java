package com.carddemo.common.copybook;

import com.carddemo.runtime.vsam.FlatFiles;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.util.List;

/** Seed records from the line-sequential datasets in app/data/ASCII (copied to the classpath by the runtime pom). */
final class AsciiSeed {

    private AsciiSeed() {
    }

    static List<String> records(String resource, int lrecl) {
        try (InputStream in = AsciiSeed.class.getClassLoader().getResourceAsStream(resource)) {
            if (in == null) {
                throw new IllegalStateException("Missing classpath resource " + resource);
            }
            return FlatFiles.readLines(in, lrecl);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
