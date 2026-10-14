package com.carddemo.common.copybook;

import com.carddemo.runtime.vsam.FlatFiles;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.util.List;

/**
 * CARDDAT seed records from {@code app/data/ASCII/carddata.txt} (50 records, LRECL 150 = CVACT02Y), the
 * ASCII form of {@code AWS.M2.CARDDEMO.CARDDATA.PS} loaded by ACCTFILE/CARDFILE.jcl.
 */
public final class CardDataSeed {

    public static final String RESOURCE = "carddemo/data/ASCII/carddata.txt";
    /** CSD: DEFINE FILE(CARDDAT) DSNAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS). */
    public static final String FILE_NAME = "CARDDAT";
    public static final String DSNAME = "AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS";
    /** CSD: DEFINE FILE(CARDAIX) DSNAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX.PATH) - path over CARDDAT by CARD-ACCT-ID. */
    public static final String AIX_FILE_NAME = "CARDAIX";

    private CardDataSeed() {
    }

    public static List<String> records() {
        try (InputStream in = CardDataSeed.class.getClassLoader().getResourceAsStream(RESOURCE)) {
            if (in == null) {
                throw new IllegalStateException("Missing classpath resource " + RESOURCE);
            }
            return FlatFiles.readLines(in, CardRecord.LENGTH);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
