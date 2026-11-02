package com.carddemo.common.record.layout;

import com.carddemo.common.record.RecordFormat;
import com.carddemo.common.record.RecordLayout;

import java.nio.file.Path;
import java.util.Optional;

/**
 * The sample datasets under {@code app/data}: the copybook that describes each, its ASCII extract
 * (line sequential, where one exists) and its EBCDIC {@code .PS} file (fixed blocks, CP037).
 * {@code asciiFormat} is the layout of the extract as committed, so writing it back reproduces the file.
 */
public enum SampleDataset {

    ACCTDATA(AccountRecordLayout.LAYOUT, "acctdata.txt", RecordFormat.LINE_SEQUENTIAL, "AWS.M2.CARDDEMO.ACCTDATA.PS"),
    CARDDATA(CardRecordLayout.LAYOUT, "carddata.txt", RecordFormat.LINE_SEQUENTIAL, "AWS.M2.CARDDEMO.CARDDATA.PS"),
    CARDXREF(CardXrefRecordLayout.LAYOUT, "cardxref.txt", RecordFormat.LINE_SEQUENTIAL.trimmingTrailingSpaces(),
            "AWS.M2.CARDDEMO.CARDXREF.PS"),
    CUSTDATA(CustomerRecordLayout.LAYOUT, "custdata.txt", RecordFormat.LINE_SEQUENTIAL, "AWS.M2.CARDDEMO.CUSTDATA.PS"),
    DALYTRAN(DalytranRecordLayout.LAYOUT, "dailytran.txt", RecordFormat.LINE_SEQUENTIAL, "AWS.M2.CARDDEMO.DALYTRAN.PS"),
    DISCGRP(DisGroupRecordLayout.LAYOUT, "discgrp.txt", RecordFormat.LINE_SEQUENTIAL, "AWS.M2.CARDDEMO.DISCGRP.PS"),
    /** {@code tcatbal.txt} ends its last line with LF and every other with CRLF. */
    TCATBALF(TranCatBalRecordLayout.LAYOUT, "tcatbal.txt", RecordFormat.LINE_SEQUENTIAL.withCrlf(),
            "AWS.M2.CARDDEMO.TCATBALF.PS"),
    TRANCATG(TranCatRecordLayout.LAYOUT, "trancatg.txt", RecordFormat.LINE_SEQUENTIAL.withCrlf(),
            "AWS.M2.CARDDEMO.TRANCATG.PS"),
    /** {@code trantype.txt} ends its last line with LF and every other with CRLF. */
    TRANTYPE(TranTypeRecordLayout.LAYOUT, "trantype.txt", RecordFormat.LINE_SEQUENTIAL.withCrlf(),
            "AWS.M2.CARDDEMO.TRANTYPE.PS"),
    USRSEC(SecUserDataLayout.LAYOUT, null, null, "AWS.M2.CARDDEMO.USRSEC.PS"),
    EXPORT(ExportRecordLayout.LAYOUT, null, null, "AWS.M2.CARDDEMO.EXPORT.DATA.PS");

    private final RecordLayout layout;
    private final String asciiFile;
    private final RecordFormat asciiFormat;
    private final String ebcdicFile;

    SampleDataset(RecordLayout layout, String asciiFile, RecordFormat asciiFormat, String ebcdicFile) {
        this.layout = layout;
        this.asciiFile = asciiFile;
        this.asciiFormat = asciiFormat;
        this.ebcdicFile = ebcdicFile;
    }

    public RecordLayout layout() {
        return layout;
    }

    /** {@code app/data/ASCII/<file>}, absent for datasets shipped only in EBCDIC. */
    public Optional<Path> asciiPath(Path dataDir) {
        return Optional.ofNullable(asciiFile).map(f -> dataDir.resolve("ASCII").resolve(f));
    }

    public Optional<RecordFormat> asciiFormat() {
        return Optional.ofNullable(asciiFormat);
    }

    public Path ebcdicPath(Path dataDir) {
        return dataDir.resolve("EBCDIC").resolve(ebcdicFile);
    }
}
