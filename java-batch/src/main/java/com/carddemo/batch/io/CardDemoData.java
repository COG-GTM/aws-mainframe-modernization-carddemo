package com.carddemo.batch.io;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Locates the ASCII sample data in {@code app/data/ASCII}. Override with the system property
 * {@code carddemo.data.dir}; otherwise {@code ../app/data/ASCII} or {@code app/data/ASCII} relative to
 * the working directory.
 */
public final class CardDemoData {

    public static final String ACCOUNTS = "acctdata.txt";
    public static final String CARD_XREF = "cardxref.txt";
    public static final String CUSTOMERS = "custdata.txt";
    public static final String DAILY_TRANSACTIONS = "dailytran.txt";
    public static final String DISCLOSURE_GROUPS = "discgrp.txt";
    public static final String TRAN_CAT_BALANCES = "tcatbal.txt";

    private CardDemoData() {
    }

    public static Path asciiDir() {
        String override = System.getProperty("carddemo.data.dir");
        if (override != null && !override.isBlank()) {
            return Paths.get(override).toAbsolutePath().normalize();
        }
        for (String candidate : new String[] {"../app/data/ASCII", "app/data/ASCII"}) {
            Path p = Paths.get(candidate).toAbsolutePath().normalize();
            if (Files.isDirectory(p)) {
                return p;
            }
        }
        throw new IllegalStateException("app/data/ASCII not found; set -Dcarddemo.data.dir");
    }

    public static Path file(String name) {
        return asciiDir().resolve(name);
    }
}
