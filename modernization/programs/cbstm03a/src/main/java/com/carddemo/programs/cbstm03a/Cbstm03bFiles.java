package com.carddemo.programs.cbstm03a;

import com.carddemo.programs.cbstm03a.copybook.AccountRecord;
import com.carddemo.programs.cbstm03a.copybook.CardXrefRecord;
import com.carddemo.programs.cbstm03a.copybook.CustomerRecord;
import com.carddemo.programs.cbstm03a.copybook.M03bArea;
import com.carddemo.programs.cbstm03a.copybook.TrnxRecord;
import com.carddemo.programs.cbstm03a.io.InMemoryRecordReader;
import com.carddemo.programs.cbstm03a.io.LineSequentialFileReader;
import com.carddemo.programs.cbstm03a.io.RecordReader;
import com.carddemo.programs.cbstm03a.support.Pic;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Behavioural equivalent of CBSTM03B over pluggable record readers. The four KSDS
 * datasets are modelled as: TRNXFILE / XREFFILE read sequentially in key order,
 * CUSTFILE / ACCTFILE read randomly by key. Every operation returns the two-byte
 * VSAM file status in WS-M03B-RC exactly where CBSTM03B's nn00-EXIT moves it.
 */
public final class Cbstm03bFiles implements Cbstm03bCall {

    /** Opens a dataset; an {@link IOException} maps to file status 35. */
    @FunctionalInterface
    public interface Opener {
        RecordReader open() throws IOException;
    }

    private final SequentialKsds trnxFile;
    private final SequentialKsds xrefFile;
    private final RandomKsds custFile;
    private final RandomKsds acctFile;

    public Cbstm03bFiles(Opener trnx, Opener xref, Opener cust, Opener acct) {
        this.trnxFile = new SequentialKsds(trnx, TrnxRecord.KEY_LENGTH);
        this.xrefFile = new SequentialKsds(xref, 16);
        this.custFile = new RandomKsds(cust, CustomerRecord.KEY_LENGTH);
        this.acctFile = new RandomKsds(acct, AccountRecord.KEY_LENGTH);
    }

    /** DD names bound to line-per-record ASCII files (app/data/ASCII format). */
    public static Cbstm03bFiles fromFiles(Path trnx, Path xref, Path cust, Path acct) {
        return new Cbstm03bFiles(
                () -> new LineSequentialFileReader(trnx, TrnxRecord.LENGTH),
                () -> new LineSequentialFileReader(xref, CardXrefRecord.LENGTH),
                () -> new LineSequentialFileReader(cust, CustomerRecord.LENGTH),
                () -> new LineSequentialFileReader(acct, AccountRecord.LENGTH));
    }

    public static Cbstm03bFiles inMemory(List<String> trnx, List<String> xref, List<String> cust, List<String> acct) {
        return new Cbstm03bFiles(
                () -> new InMemoryRecordReader(TrnxRecord.LENGTH, trnx),
                () -> new InMemoryRecordReader(CardXrefRecord.LENGTH, xref),
                () -> new InMemoryRecordReader(CustomerRecord.LENGTH, cust),
                () -> new InMemoryRecordReader(AccountRecord.LENGTH, acct));
    }

    // 0000-START
    @Override
    public void call(M03bArea area) {
        switch (area.dd()) {
            case "TRNXFILE" -> trnxfileProc1000(area);
            case "XREFFILE" -> xreffileProc2000(area);
            case "CUSTFILE" -> custfileProc3000(area);
            case "ACCTFILE" -> acctfileProc4000(area);
            default -> {
                // WHEN OTHER: GO TO 9999-GOBACK, LK-M03B-RC untouched
            }
        }
    }

    // 1000-TRNXFILE-PROC THRU 1999-EXIT
    private void trnxfileProc1000(M03bArea area) {
        sequential(trnxFile, area);
    }

    // 2000-XREFFILE-PROC THRU 2999-EXIT
    private void xreffileProc2000(M03bArea area) {
        sequential(xrefFile, area);
    }

    // 3000-CUSTFILE-PROC THRU 3999-EXIT
    private void custfileProc3000(M03bArea area) {
        random(custFile, area);
    }

    // 4000-ACCTFILE-PROC THRU 4999-EXIT
    private void acctfileProc4000(M03bArea area) {
        random(acctFile, area);
    }

    private static void sequential(SequentialKsds file, M03bArea area) {
        if (area.oper() == M03bArea.Oper.OPEN) {
            file.open();
        } else if (area.oper() == M03bArea.Oper.READ) {
            file.read().ifPresent(area::fldt);
        } else if (area.oper() == M03bArea.Oper.CLOSE) {
            file.close();
        }
        area.rc(file.status);
    }

    private static void random(RandomKsds file, M03bArea area) {
        if (area.oper() == M03bArea.Oper.OPEN) {
            file.open();
        } else if (area.oper() == M03bArea.Oper.READ_K) {
            String key = area.key().substring(0, Math.max(0, Math.min(area.keyLn(), 25)));
            file.read(Pic.x(key, file.keyLength)).ifPresent(area::fldt);
        } else if (area.oper() == M03bArea.Oper.CLOSE) {
            file.close();
        }
        area.rc(file.status);
    }

    private static final class SequentialKsds {
        private final Opener opener;
        private final int keyLength;
        private Iterator<String> cursor;
        private boolean atEnd;
        private String status = "00";

        SequentialKsds(Opener opener, int keyLength) {
            this.opener = opener;
            this.keyLength = keyLength;
        }

        void open() {
            if (cursor != null) {
                status = "41";
                return;
            }
            List<String> records = new ArrayList<>();
            try (RecordReader reader = opener.open()) {
                for (Optional<String> r = reader.read(); r.isPresent(); r = reader.read()) {
                    records.add(r.get());
                }
            } catch (IOException e) {
                status = "35";
                return;
            }
            records.sort(Comparator.comparing(r -> r.substring(0, keyLength)));
            cursor = records.iterator();
            atEnd = false;
            status = "00";
        }

        Optional<String> read() {
            if (cursor == null) {
                status = "47";
                return Optional.empty();
            }
            if (atEnd) {
                status = "46";
                return Optional.empty();
            }
            if (!cursor.hasNext()) {
                atEnd = true;
                status = "10";
                return Optional.empty();
            }
            status = "00";
            return Optional.of(cursor.next());
        }

        void close() {
            status = cursor == null ? "42" : "00";
            cursor = null;
        }
    }

    private static final class RandomKsds {
        private final Opener opener;
        private final int keyLength;
        private Map<String, String> records;
        private String status = "00";

        RandomKsds(Opener opener, int keyLength) {
            this.opener = opener;
            this.keyLength = keyLength;
        }

        void open() {
            if (records != null) {
                status = "41";
                return;
            }
            Map<String, String> loaded = new HashMap<>();
            try (RecordReader reader = opener.open()) {
                for (Optional<String> r = reader.read(); r.isPresent(); r = reader.read()) {
                    loaded.putIfAbsent(r.get().substring(0, keyLength), r.get());
                }
            } catch (IOException e) {
                status = "35";
                return;
            }
            records = loaded;
            status = "00";
        }

        Optional<String> read(String key) {
            if (records == null) {
                status = "47";
                return Optional.empty();
            }
            String record = records.get(key);
            status = record == null ? "23" : "00";
            return Optional.ofNullable(record);
        }

        void close() {
            status = records == null ? "42" : "00";
            records = null;
        }
    }
}
