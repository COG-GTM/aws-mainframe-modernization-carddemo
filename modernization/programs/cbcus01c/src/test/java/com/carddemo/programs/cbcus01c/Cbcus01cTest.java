package com.carddemo.programs.cbcus01c;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;

class Cbcus01cTest {

    private static final String START = "START OF EXECUTION OF PROGRAM CBCUS01C";
    private static final String END = "END OF EXECUTION OF PROGRAM CBCUS01C";

    private final List<String> sysout = new ArrayList<>();

    private Cbcus01c program(CustomerFile file) {
        return new Cbcus01c(file, sysout::add);
    }

    // PROCEDURE DIVISION main loop + 1000-CUSTFILE-GET-NEXT: every record is DISPLAYed twice
    @Test
    void displaysEachRecordTwiceBetweenStartAndEndBanners() {
        String a = Fixtures.customer("000000001", "ALPHA");
        String b = Fixtures.customer("000000002", "BRAVO");
        InMemoryCustomerFile file = new InMemoryCustomerFile(List.of(a, b));

        int rc = program(file).run();

        assertEquals(0, rc);
        assertEquals(List.of(START, a, a, b, b, END), sysout);
        assertFalse(file.isOpen());
    }

    @Test
    void readsInRecordKeySequenceLikeKsds() {
        String a = Fixtures.customer("000000001", "ALPHA");
        String b = Fixtures.customer("000000002", "BRAVO");

        program(new InMemoryCustomerFile(List.of(b, a))).run();

        assertEquals(List.of(START, a, a, b, b, END), sysout);
    }

    @Test
    void emptyFileHitsEndOfFileImmediately() {
        assertEquals(0, program(new InMemoryCustomerFile(List.of())).run());
        assertEquals(List.of(START, END), sysout);
    }

    // READ ... INTO CUSTOMER-RECORD pads a short record to 500 bytes
    @Test
    void readIntoMovesFullFiveHundredByteImage() {
        program(new InMemoryCustomerFile(List.of("000000007SHORT"))).run();
        assertEquals(500, sysout.get(1).length());
        assertEquals("000000007SHORT" + " ".repeat(486), sysout.get(1));
    }

    @Test
    void customerRecordExposesTypedCopybookView() {
        Cbcus01c p = program(new InMemoryCustomerFile(List.of(Fixtures.customer("000000123", "ZED"))));
        p.custfileOpen();
        p.custfileGetNext();
        assertEquals(123L, p.customerRecord().custId());
        assertEquals("ZED", p.customerRecord().custFirstName().strip());
    }

    // 0000-CUSTFILE-OPEN
    @Test
    void openSuccessSetsApplAok() {
        Cbcus01c p = program(new InMemoryCustomerFile(List.of()));
        p.custfileOpen();
        assertEquals(0, p.applResult());
        assertEquals(List.of(), sysout);
    }

    @Test
    void openFailureAbendsWith999() {
        AbendException e = assertThrows(AbendException.class,
                () -> program(new InMemoryCustomerFile(List.of()).withOpenStatus("35")).run());

        assertEquals(999, e.abendCode());
        assertEquals(0, e.timing());
        assertEquals(List.of(START, "ERROR OPENING CUSTFILE", "FILE STATUS IS: NNNN0035", "ABENDING PROGRAM"),
                sysout);
    }

    // 1000-CUSTFILE-GET-NEXT
    @Test
    void endOfFileSetsApplEofAndEndOfFileFlag() {
        Cbcus01c p = program(new InMemoryCustomerFile(List.of()));
        p.custfileOpen();
        p.custfileGetNext();
        assertEquals(16, p.applResult());
    }

    @Test
    void readErrorAbendsAfterRecordsAlreadyDisplayed() {
        String a = Fixtures.customer("000000001", "ALPHA");
        CustomerFile file = new InMemoryCustomerFile(List.of(a, Fixtures.customer("000000002", "B")))
                .withReadStatus(2, "30");

        AbendException e = assertThrows(AbendException.class, () -> program(file).run());

        assertEquals(999, e.abendCode());
        assertEquals(List.of(START, a, a, "ERROR READING CUSTOMER FILE", "FILE STATUS IS: NNNN0030",
                "ABENDING PROGRAM"), sysout);
    }

    @Test
    void nonZeroSuccessStatusIsTreatedAsReadError() {
        CustomerFile file = new InMemoryCustomerFile(List.of()).withReadStatus(1, "04");

        assertThrows(AbendException.class, () -> program(file).run());

        assertEquals(List.of(START, "ERROR READING CUSTOMER FILE", "FILE STATUS IS: NNNN0004",
                "ABENDING PROGRAM"), sysout);
    }

    // 9000-CUSTFILE-CLOSE
    @Test
    void closeFailureAbends() {
        CustomerFile file = new InMemoryCustomerFile(List.of()).withCloseStatus("42");

        AbendException e = assertThrows(AbendException.class, () -> program(file).run());

        assertEquals(999, e.abendCode());
        assertEquals(List.of(START, "ERROR CLOSING CUSTOMER FILE", "FILE STATUS IS: NNNN0042",
                "ABENDING PROGRAM"), sysout);
    }

    // Z-DISPLAY-IO-STATUS
    @Test
    void ioStatusStartingWithNineShowsBinaryValueOfSecondByteInEbcdic() {
        CustomerFile file = new InMemoryCustomerFile(List.of()).withOpenStatus("93");
        assertThrows(AbendException.class, () -> program(file).run());
        // EBCDIC '3' = X'F3' = 243
        assertEquals("FILE STATUS IS: NNNN9243", sysout.get(2));
    }

    @Test
    void ioStatusDigitsIgnoreDefaultLocale() {
        java.util.Locale saved = java.util.Locale.getDefault();
        try {
            java.util.Locale.setDefault(java.util.Locale.forLanguageTag("ar-EG"));
            CustomerFile file = new InMemoryCustomerFile(List.of()).withOpenStatus("93");
            assertThrows(AbendException.class, () -> program(file).run());
            assertEquals("FILE STATUS IS: NNNN9243", sysout.get(2));
        } finally {
            java.util.Locale.setDefault(saved);
        }
    }

    @Test
    void ioStatusBinaryValueFollowsConfiguredCharset() {
        CustomerFile file = new InMemoryCustomerFile(List.of()).withOpenStatus("93");
        Cbcus01c p = new Cbcus01c(file, sysout::add, StandardCharsets.US_ASCII);
        assertThrows(AbendException.class, p::run);
        // ASCII '3' = X'33' = 51
        assertEquals("FILE STATUS IS: NNNN9051", sysout.get(2));
    }

    @Test
    void nonNumericIoStatusUsesBinaryBranch() {
        CustomerFile file = new InMemoryCustomerFile(List.of()).withOpenStatus("0A");
        assertThrows(AbendException.class, () -> program(file).run());
        // EBCDIC 'A' = X'C1' = 193
        assertEquals("FILE STATUS IS: NNNN0193", sysout.get(2));
    }

    @Test
    void numericIoStatusIsZeroPrefixed() {
        CustomerFile file = new InMemoryCustomerFile(List.of()).withOpenStatus("39");
        assertThrows(AbendException.class, () -> program(file).run());
        assertEquals("FILE STATUS IS: NNNN0039", sysout.get(2));
    }

    // Fixture-based: app/data/ASCII/custdata.txt through the file-backed KSDS reader
    @Test
    void sampleCustomerFileIsPrintedInFull() throws IOException {
        List<String> lines = Fixtures.custdataLines();

        int rc = program(new KsdsCustomerFile(Fixtures.CUSTDATA)).run();

        assertEquals(0, rc);
        assertEquals(2 + 2 * lines.size(), sysout.size());
        assertEquals(START, sysout.get(0));
        for (int i = 0; i < lines.size(); i++) {
            assertEquals(lines.get(i), sysout.get(1 + 2 * i));
            assertEquals(lines.get(i), sysout.get(2 + 2 * i));
        }
        assertEquals("000000001", sysout.get(1).substring(0, 9));
        assertEquals("000000050", sysout.get(sysout.size() - 2).substring(0, 9));
        assertEquals(END, sysout.get(sysout.size() - 1));
    }

    @Test
    void missingCustfileAbendsWithStatus35() {
        CustomerFile file = new KsdsCustomerFile(Path.of("does-not-exist.txt"));
        assertThrows(AbendException.class, () -> program(file).run());
        assertEquals("FILE STATUS IS: NNNN0035", sysout.get(2));
    }

    // Fixture-based: RECFM=FB EBCDIC feed loaded by app/jcl/CUSTFILE.jcl
    @Test
    void ebcdicFixedBlockFeedPrintsSameRecordsAsAsciiSample() throws IOException {
        Path ebcdic = Fixtures.CUSTDATA.resolve("../../EBCDIC/AWS.M2.CARDDEMO.CUSTDATA.PS").normalize();
        List<String> lines = Fixtures.custdataLines();

        int rc = program(KsdsCustomerFile.fixedBlock(ebcdic, Cbcus01c.EBCDIC)).run();

        assertEquals(0, rc);
        assertEquals(2 + 2 * lines.size(), sysout.size());
        for (int i = 0; i < lines.size(); i++) {
            assertEquals(lines.get(i), sysout.get(1 + 2 * i));
        }
    }

    @Test
    void sysoutWriteFailureIsNotReportedAsSuccess() {
        java.io.PrintStream broken = new java.io.PrintStream(new java.io.OutputStream() {
            @Override
            public void write(int b) throws IOException {
                throw new IOException("disk full");
            }
        });
        CustomerFile file = new InMemoryCustomerFile(List.of(Fixtures.customer("000000001", "A")));
        assertEquals(Cbcus01cMain.RC_SYSOUT_ERROR, Cbcus01cMain.execute(file, broken));
    }

    @Test
    void mainReturnsZeroForSampleAndAbendCodeForMissingFile() {
        assertEquals(0, Cbcus01cMain.execute(Fixtures.CUSTDATA));
        assertEquals(999, Cbcus01cMain.execute(Path.of("does-not-exist.txt")));
    }

    @Test
    void invalidOrMultibyteCharsetIsAUsageError() {
        java.io.PrintStream nul = new java.io.PrintStream(java.io.OutputStream.nullOutputStream());
        String f = Fixtures.CUSTDATA.toString();
        assertEquals(Cbcus01cMain.RC_USAGE, Cbcus01cMain.run(new String[] {"--charset=", f}, nul, nul));
        assertEquals(Cbcus01cMain.RC_USAGE, Cbcus01cMain.run(new String[] {"--charset=NO-SUCH-CS", f}, nul, nul));
        assertEquals(Cbcus01cMain.RC_USAGE, Cbcus01cMain.run(new String[] {"--charset=UTF-8", f}, nul, nul));
        assertEquals(Cbcus01cMain.RC_USAGE, Cbcus01cMain.run(new String[] {"--charset=ISO-2022-CN", f}, nul, nul));
        assertEquals(Cbcus01cMain.RC_USAGE,
                Cbcus01cMain.run(new String[] {"--fixed-block", "--charset=ISO-2022-CN", f}, nul, nul));
        assertEquals(Cbcus01cMain.RC_USAGE, Cbcus01cMain.run(new String[] {"--fixed-block"}, nul, nul));
    }

    @Test
    void cliFixedBlockDefaultsToEbcdic() throws IOException {
        Path ebcdic = Fixtures.CUSTDATA.resolve("../../EBCDIC/AWS.M2.CARDDEMO.CUSTDATA.PS").normalize();
        java.io.ByteArrayOutputStream out = new java.io.ByteArrayOutputStream();
        java.io.PrintStream nul = new java.io.PrintStream(java.io.OutputStream.nullOutputStream());

        int rc = Cbcus01cMain.run(new String[] {"--fixed-block", ebcdic.toString()},
                new java.io.PrintStream(out, true, StandardCharsets.US_ASCII), nul);

        assertEquals(0, rc);
        List<String> printed = out.toString(StandardCharsets.US_ASCII).lines().toList();
        List<String> lines = Fixtures.custdataLines();
        assertEquals(2 + 2 * lines.size(), printed.size());
        for (int i = 0; i < lines.size(); i++) {
            assertEquals(lines.get(i), printed.get(1 + 2 * i));
        }
    }

    @Test
    void interruptedSysoutWriteIsNotReportedAsSuccess() {
        java.io.PrintStream interrupted = new java.io.PrintStream(new java.io.OutputStream() {
            @Override
            public void write(int b) throws IOException {
                throw new java.io.InterruptedIOException("interrupted");
            }
        });
        CustomerFile file = new InMemoryCustomerFile(List.of(Fixtures.customer("000000001", "A")));
        try {
            assertEquals(Cbcus01cMain.RC_SYSOUT_ERROR, Cbcus01cMain.execute(file, interrupted));
            assertTrue(Thread.currentThread().isInterrupted());
        } finally {
            Thread.interrupted();
        }
    }

    @Test
    void callerInterruptIsPreservedAndNotMistakenForSysoutFailure() {
        java.io.PrintStream healthy = new java.io.PrintStream(java.io.OutputStream.nullOutputStream());
        CustomerFile file = new InMemoryCustomerFile(List.of(Fixtures.customer("000000001", "A")));
        Thread.currentThread().interrupt();
        try {
            assertEquals(0, Cbcus01cMain.execute(file, healthy));
            assertTrue(Thread.currentThread().isInterrupted());
        } finally {
            Thread.interrupted();
        }
    }

    @Test
    void interruptedSysoutWriteIsDetectedEvenIfCallerWasAlreadyInterrupted() {
        java.io.PrintStream interrupted = new java.io.PrintStream(new java.io.OutputStream() {
            @Override
            public void write(int b) throws IOException {
                throw new java.io.InterruptedIOException("interrupted");
            }
        });
        CustomerFile file = new InMemoryCustomerFile(List.of(Fixtures.customer("000000001", "A")));
        Thread.currentThread().interrupt();
        try {
            assertEquals(Cbcus01cMain.RC_SYSOUT_ERROR, Cbcus01cMain.execute(file, interrupted));
            assertTrue(Thread.currentThread().isInterrupted());
        } finally {
            Thread.interrupted();
        }
    }

    @Test
    void callerInterruptIsRestoredWhenRunThrows() {
        java.io.PrintStream healthy = new java.io.PrintStream(java.io.OutputStream.nullOutputStream());
        CustomerFile file = new CustomerFile() {
            @Override
            public String open() {
                throw new IllegalStateException("boom");
            }

            @Override
            public ReadResult readNext() {
                return ReadResult.status(FileStatus.END_OF_FILE);
            }

            @Override
            public String close() {
                return FileStatus.SUCCESS;
            }
        };
        Thread.currentThread().interrupt();
        try {
            assertThrows(IllegalStateException.class, () -> Cbcus01cMain.execute(file, healthy));
            assertTrue(Thread.currentThread().isInterrupted());
        } finally {
            Thread.interrupted();
        }
    }
}
