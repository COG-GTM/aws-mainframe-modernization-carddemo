package com.carddemo.support;

import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.TestData;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/** The shipped sample datasets under app/data (TRANSACT has none). */
public final class Samples {

    private static final Map<Dataset, String> ASCII_TWINS = Map.of(Dataset.TRANTYPE, "trantype.txt",
            Dataset.TRANCATG, "trancatg.txt", Dataset.DISCGRP, "discgrp.txt", Dataset.CUSTDATA, "custdata.txt",
            Dataset.ACCTDATA, "acctdata.txt", Dataset.CARDDATA, "carddata.txt", Dataset.CARDXREF, "cardxref.txt",
            Dataset.TCATBALF, "tcatbal.txt", Dataset.DALYTRAN, "dailytran.txt");

    private Samples() {
    }

    public static List<Dataset> withEbcdicSample() {
        return Arrays.stream(Dataset.values()).filter(d -> d != Dataset.TRANSACT).toList();
    }

    public static List<Dataset> withAsciiSample() {
        return withEbcdicSample().stream().filter(ASCII_TWINS::containsKey).toList();
    }

    public static Path path(Dataset dataset, RecordEncoding encoding) {
        return encoding == RecordEncoding.EBCDIC
                ? TestData.resolve("app/data/EBCDIC/AWS.M2.CARDDEMO." + dataset + ".PS")
                : TestData.resolve("app/data/ASCII/" + ASCII_TWINS.get(dataset));
    }

    public static List<FixedWidthRecord> read(Dataset dataset, RecordEncoding encoding) {
        return dataset.read(path(dataset, encoding), encoding);
    }
}
