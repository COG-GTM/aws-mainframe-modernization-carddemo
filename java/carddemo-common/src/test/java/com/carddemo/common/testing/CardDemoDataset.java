package com.carddemo.common.testing;

import com.carddemo.common.cobol.DataEncoding;
import com.carddemo.common.copybook.RecordLayout;
import com.carddemo.common.io.RecordFileReader;
import com.carddemo.common.io.RecordFormat;

import java.util.List;
import java.util.Optional;

/**
 * The sample datasets shipped in {@code app/data}, with their copybook and LRECL (README "Upload Sample Data").
 * The EBCDIC file is the binary mainframe dataset; the ASCII file, when present, is its text-mode transfer.
 */
public enum CardDemoDataset {
    USRSEC("AWS.M2.CARDDEMO.USRSEC.PS", null, "CSUSR01Y", 80),
    ACCTDATA("AWS.M2.CARDDEMO.ACCTDATA.PS", "acctdata.txt", "CVACT01Y", 300),
    CARDDATA("AWS.M2.CARDDEMO.CARDDATA.PS", "carddata.txt", "CVACT02Y", 150),
    CUSTDATA("AWS.M2.CARDDEMO.CUSTDATA.PS", "custdata.txt", "CVCUS01Y", 500),
    CARDXREF("AWS.M2.CARDDEMO.CARDXREF.PS", "cardxref.txt", "CVACT03Y", 50),
    DALYTRAN("AWS.M2.CARDDEMO.DALYTRAN.PS", "dailytran.txt", "CVTRA06Y", 350),
    DISCGRP("AWS.M2.CARDDEMO.DISCGRP.PS", "discgrp.txt", "CVTRA02Y", 50),
    TCATBALF("AWS.M2.CARDDEMO.TCATBALF.PS", "tcatbal.txt", "CVTRA01Y", 50),
    TRANCATG("AWS.M2.CARDDEMO.TRANCATG.PS", "trancatg.txt", "CVTRA04Y", 60),
    TRANTYPE("AWS.M2.CARDDEMO.TRANTYPE.PS", "trantype.txt", "CVTRA03Y", 60);

    private final String ebcdicFile;
    private final String asciiFile;
    private final String copybook;
    private final int lrecl;

    CardDemoDataset(String ebcdicFile, String asciiFile, String copybook, int lrecl) {
        this.ebcdicFile = ebcdicFile;
        this.asciiFile = asciiFile;
        this.copybook = copybook;
        this.lrecl = lrecl;
    }

    public String ebcdicFile() {
        return ebcdicFile;
    }

    public Optional<String> asciiFile() {
        return Optional.ofNullable(asciiFile);
    }

    public String copybook() {
        return copybook;
    }

    public int lrecl() {
        return lrecl;
    }

    public RecordLayout layout(MainframeSources sources) {
        return sources.layout(copybook);
    }

    /** Records of the binary EBCDIC dataset (RECFM=FB). */
    public List<byte[]> ebcdicRecords(MainframeSources sources) {
        return RecordFileReader.readAll(sources.ebcdicData(ebcdicFile), RecordFormat.FIXED, lrecl, DataEncoding.EBCDIC);
    }

    /** Records of the ASCII text transfer, padded back to LRECL. */
    public List<byte[]> asciiRecords(MainframeSources sources) {
        String file = asciiFile().orElseThrow(() -> new IllegalStateException(name() + " has no ASCII copy"));
        return RecordFileReader.readAll(sources.asciiData(file), RecordFormat.LINE, lrecl, DataEncoding.ASCII);
    }
}
