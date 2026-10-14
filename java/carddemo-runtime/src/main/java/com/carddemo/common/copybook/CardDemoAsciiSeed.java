package com.carddemo.common.copybook;

import com.carddemo.runtime.vsam.FlatFiles;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.util.List;

/**
 * Seed records from the ASCII datasets under {@code app/data/ASCII} (copied to the classpath as
 * {@code carddemo/data/ASCII/<file>} by the runtime pom). Each line is one record padded to LRECL.
 */
public final class CardDemoAsciiSeed {

    public static final String RESOURCE_DIR = "carddemo/data/ASCII/";

    private CardDemoAsciiSeed() {
    }

    public static List<String> records(String fileName, int lrecl) {
        String resource = RESOURCE_DIR + fileName;
        try (InputStream in = CardDemoAsciiSeed.class.getClassLoader().getResourceAsStream(resource)) {
            if (in == null) {
                throw new IllegalStateException("Missing classpath resource " + resource);
            }
            return FlatFiles.readLines(in, lrecl);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
