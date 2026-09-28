package com.carddemo.batch.storage;

import java.time.LocalDate;

/** S3 key layout from batch.md §1.2. */
public final class S3Keys {

    private S3Keys() {
    }

    public static String dailyTransactions(LocalDate d) {
        return "input/dalytran/" + d + "/dalytran.txt";
    }

    public static String dailyRejects(LocalDate d, String runId) {
        return "output/dalyrejs/" + d + "/" + runId + ".txt";
    }

    public static String backup(String table, LocalDate d, String runId) {
        return "backup/" + table + "/" + d + "/" + runId + ".csv.gz";
    }

    public static String backupPrefix(String table) {
        return "backup/" + table + "/";
    }

    public static String systemTransactions(LocalDate d, String runId) {
        return "output/systran/" + d + "/" + runId + ".txt";
    }

    public static String systemTransactionsPrefix() {
        return "output/systran/";
    }

    public static String transactionReport(LocalDate d, String runId) {
        return "reports/tranrept/" + d + "/" + runId + ".txt";
    }

    public static String statement(LocalDate d, String runId, String ext) {
        return "statements/" + d + "/" + runId + "/statement." + ext;
    }

    public static String refdata(String table, String runId) {
        return "refdata/" + table + "/" + runId + ".txt";
    }

    public static String refdataPrefix(String table) {
        return "refdata/" + table + "/";
    }

    public static String seedAscii(String file) {
        return "seed/ascii/" + file;
    }

    public static String transactionTypeMaintenance(LocalDate d) {
        return "input/trantype-maint/" + d + "/maint.txt";
    }

    public static String runResult(String runId, String jobName) {
        return "runs/" + runId + "/" + jobName + ".json";
    }
}
