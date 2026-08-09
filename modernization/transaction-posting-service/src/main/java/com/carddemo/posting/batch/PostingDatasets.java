package com.carddemo.posting.batch;

import com.carddemo.posting.exception.DatasetAccessException;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * The dataset images the posting job reads, one per input DD of job {@code POSTTRAN}
 * ({@code app/jcl/POSTTRAN.jcl:26-43}).
 *
 * <p>Holding images rather than paths keeps the job itself free of file handling and lets tests
 * drive it with synthetic populations.
 */
public record PostingDatasets(byte[] dailyTransactionImage,
                              byte[] cardXrefImage,
                              byte[] accountImage,
                              byte[] categoryBalanceImage) {

    /** {@code DALYTRAN} DD ({@code app/jcl/POSTTRAN.jcl:30-31}). */
    public static final String DALYTRAN_DATASET = "AWS.M2.CARDDEMO.DALYTRAN.PS";
    /** {@code XREFFILE} DD ({@code app/jcl/POSTTRAN.jcl:32-33}). */
    public static final String CARDXREF_DATASET = "AWS.M2.CARDDEMO.CARDXREF.PS";
    /** {@code ACCTFILE} DD ({@code app/jcl/POSTTRAN.jcl:40-41}). */
    public static final String ACCTFILE_DATASET = "AWS.M2.CARDDEMO.ACCTDATA.PS";
    /** {@code TCATBALF} DD ({@code app/jcl/POSTTRAN.jcl:42-43}). */
    public static final String TCATBALF_DATASET = "AWS.M2.CARDDEMO.TCATBALF.PS";

    /** Loads the four inputs from a directory of unloaded EBCDIC datasets. */
    public static PostingDatasets loadFrom(Path directory) {
        return new PostingDatasets(
                read(directory, DALYTRAN_DATASET),
                read(directory, CARDXREF_DATASET),
                read(directory, ACCTFILE_DATASET),
                read(directory, TCATBALF_DATASET));
    }

    private static byte[] read(Path directory, String datasetName) {
        try {
            return Files.readAllBytes(directory.resolve(datasetName));
        } catch (IOException e) {
            throw new DatasetAccessException("Cannot read dataset " + datasetName, e);
        }
    }
}
