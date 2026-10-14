package com.carddemo.common.copybook;

import com.carddemo.runtime.vsam.FlatFiles;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.util.List;

/**
 * Seed records from app/data/ASCII (packaged under {@value #RESOURCE_DIR}): one record per line, padded with
 * spaces to the record length (the ASCII extracts drop trailing FILLER spaces, e.g. cardxref.txt is 36 bytes
 * per line for a 50-byte record). Signed zoned fields use the ASCII overpunch convention ({ A-I } J-R).
 */
public final class AsciiSeedData {

    public static final String RESOURCE_DIR = "carddemo/data/ASCII/";
    public static final String ACCTDATA = "acctdata.txt";
    public static final String CUSTDATA = "custdata.txt";
    public static final String CARDXREF = "cardxref.txt";

    private AsciiSeedData() {
    }

    public static List<String> records(String fileName, int recordLength) {
        String resource = RESOURCE_DIR + fileName;
        try (InputStream in = AsciiSeedData.class.getClassLoader().getResourceAsStream(resource)) {
            if (in == null) {
                throw new IllegalStateException("Missing classpath resource " + resource);
            }
            return FlatFiles.readLines(in, recordLength);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
