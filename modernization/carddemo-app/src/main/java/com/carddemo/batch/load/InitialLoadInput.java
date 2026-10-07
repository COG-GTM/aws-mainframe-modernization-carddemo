package com.carddemo.batch.load;

import com.carddemo.batch.load.VsamDatasetLoader.Dataset;

/**
 * The eleven sample files the {@code initial-load} job reads from {@code app/data/EBCDIC}, in load order (parents
 * before children), with the JCL job each replaces. {@code AWS.M2.CARDDEMO.ACCDATA.PS} is byte-identical to
 * {@code ACCTDATA.PS} and is not read; {@code EXPORT.DATA.PS} is {@code cbimport} input, not a table image.
 */
public enum InitialLoadInput {

    USRSEC(Dataset.USRSEC, "DUSRSECJ", "AWS.M2.CARDDEMO.USRSEC.PS"),
    TRANTYPE(Dataset.TRANTYPE, "TRANTYPE", "AWS.M2.CARDDEMO.TRANTYPE.PS"),
    TRANCATG(Dataset.TRANCATG, "TRANCATG", "AWS.M2.CARDDEMO.TRANCATG.PS"),
    DISCGRP(Dataset.DISCGRP, "DISCGRP", "AWS.M2.CARDDEMO.DISCGRP.PS"),
    CUSTDATA(Dataset.CUSTDATA, "CUSTFILE", "AWS.M2.CARDDEMO.CUSTDATA.PS"),
    ACCTDATA(Dataset.ACCTDATA, "ACCTFILE", "AWS.M2.CARDDEMO.ACCTDATA.PS"),
    CARDDATA(Dataset.CARDDATA, "CARDFILE", "AWS.M2.CARDDEMO.CARDDATA.PS"),
    CARDXREF(Dataset.CARDXREF, "XREFFILE", "AWS.M2.CARDDEMO.CARDXREF.PS"),
    TCATBALF(Dataset.TCATBALF, "TCATBALF", "AWS.M2.CARDDEMO.TCATBALF.PS"),
    /** TRANFILE seeds the TRANSACT KSDS with a single all-LOW-VALUES priming record; it loads as zero rows. */
    TRANSACT(Dataset.TRANSACT, "TRANFILE", "AWS.M2.CARDDEMO.DALYTRAN.PS.INIT"),
    /** The POSTTRAN input; no setup job copies it, it is read where it lies on the mainframe. */
    DALYTRAN(Dataset.DALYTRAN, "POSTTRAN (DALYTRAN DD)", "AWS.M2.CARDDEMO.DALYTRAN.PS");

    private final Dataset dataset;
    private final String jclJob;
    private final String fileName;

    InitialLoadInput(Dataset dataset, String jclJob, String fileName) {
        this.dataset = dataset;
        this.jclJob = jclJob;
        this.fileName = fileName;
    }

    public Dataset dataset() {
        return dataset;
    }

    public String jclJob() {
        return jclJob;
    }

    public String fileName() {
        return fileName;
    }

    public String stepName() {
        return "load-" + name().toLowerCase(java.util.Locale.ROOT);
    }
}
