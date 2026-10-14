package com.carddemo.common.copybook;

import com.carddemo.runtime.vsam.FlatFiles;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.util.List;

/**
 * Seed records for the CardDemo master files, read from the original line-sequential datasets in
 * {@code app/data/ASCII} (copied to the classpath by the carddemo-runtime pom). Each line is padded to LRECL
 * (line-sequential files drop trailing spaces).
 */
public final class CardDemoDatasets {

    public static final String ASCII_DIR = "carddemo/data/ASCII/";

    /** CSD DEFINE FILE(ACCTDAT) DSNAME(AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS). */
    public static final String ACCTDAT = "ACCTDAT";
    public static final String ACCTDAT_DSN = "AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS";
    /** CSD DEFINE FILE(CUSTDAT) DSNAME(AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS). */
    public static final String CUSTDAT = "CUSTDAT";
    public static final String CUSTDAT_DSN = "AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS";
    /** CSD DEFINE FILE(CCXREF) DSNAME(AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS). */
    public static final String CCXREF = "CCXREF";
    public static final String CCXREF_DSN = "AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS";
    /** CSD DEFINE FILE(CXACAIX) DSNAME(AWS.M2.CARDDEMO.CARDXREF.VSAM.AIX.PATH): CCXREF by XREF-ACCT-ID. */
    public static final String CXACAIX = "CXACAIX";

    private CardDemoDatasets() {
    }

    /** app/data/ASCII/acctdata.txt as 300-byte CVACT01Y records. */
    public static List<String> accounts() {
        return ascii("acctdata.txt", AccountRecord.LENGTH);
    }

    /** app/data/ASCII/custdata.txt as 500-byte CVCUS01Y records. */
    public static List<String> customers() {
        return ascii("custdata.txt", CustomerRecord.LENGTH);
    }

    /** app/data/ASCII/cardxref.txt as 50-byte CVACT03Y records. */
    public static List<String> cardXrefs() {
        return ascii("cardxref.txt", CardXrefRecord.LENGTH);
    }

    /** Any app/data/ASCII file, each line padded/truncated to {@code lrecl}. */
    public static List<String> ascii(String fileName, int lrecl) {
        String resource = ASCII_DIR + fileName;
        try (InputStream in = CardDemoDatasets.class.getClassLoader().getResourceAsStream(resource)) {
            if (in == null) {
                throw new IllegalStateException("Missing classpath resource " + resource);
            }
            return FlatFiles.readLines(in, lrecl);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
