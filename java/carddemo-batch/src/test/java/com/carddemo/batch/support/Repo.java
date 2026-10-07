package com.carddemo.batch.support;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/** Locates the repository root (set by the pom as {@code carddemo.repo.root}) and the shared fixtures. */
public final class Repo {

    public static final Path ROOT = root();
    public static final Path SAMPLE_ACCTDATA = ROOT.resolve("app/data/ASCII/acctdata.txt");
    public static final Path GOLDEN_CBACT01C = ROOT.resolve("golden-files/CBACT01C");
    public static final Path GOLDEN_SYNTHETIC = GOLDEN_CBACT01C.resolve("synthetic-mixed-debit");
    public static final Path SAMPLE_DATA = ROOT.resolve("app/data/ASCII");
    public static final Path SAMPLE_DAILYTRAN = SAMPLE_DATA.resolve("dailytran.txt");
    public static final Path SAMPLE_CARDXREF = SAMPLE_DATA.resolve("cardxref.txt");
    public static final Path SAMPLE_CUSTDATA = SAMPLE_DATA.resolve("custdata.txt");
    public static final Path SAMPLE_CARDDATA = SAMPLE_DATA.resolve("carddata.txt");
    public static final Path GOLDEN_CBTRN01C = ROOT.resolve("golden-files/CBTRN01C");
    public static final Path GOLDEN_SYNTHETIC_REJECTIONS = GOLDEN_CBTRN01C.resolve("synthetic-rejections");
    public static final Path RECONCILE_PY = ROOT.resolve("test-harness/reconcile.py");

    private Repo() {
    }

    private static Path root() {
        String prop = System.getProperty("carddemo.repo.root");
        Path p = prop != null ? Paths.get(prop) : Paths.get("..", "..");
        p = p.toAbsolutePath().normalize();
        if (!Files.isDirectory(p.resolve("golden-files"))) {
            throw new IllegalStateException("repository root not found at " + p + " (set -Dcarddemo.repo.root)");
        }
        return p;
    }
}
