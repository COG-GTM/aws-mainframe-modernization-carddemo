package com.carddemo.batch.creastmt;

import com.carddemo.account.AccountRecord;
import com.carddemo.batch.harness.BufferedSink;
import com.carddemo.batch.harness.KeyedDataset;
import com.carddemo.batch.harness.KsdsInput;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.TestData;
import com.carddemo.customer.CustomerRecord;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Supplier;

/** CBSTM03A over files: TRNXFILE records, a CARDXREF line file and the baseline CUSTDATA / ACCTDATA after-images. */
final class CreastmtTestSupport {

    static final RecordEncoding ASCII = RecordEncoding.ASCII;
    /** What the CREASTMT baseline run read: CUSTFILE's load and INTCALC's ACCTDATA after-image. */
    static final Path CUSTDATA = TestData.resolve("docs/validation/baseline/CUSTFILE/CUSTDATA.ksds.txt");
    static final Path ACCTDATA = TestData.resolve("docs/validation/baseline/INTCALC/ACCTDATA.ksds.txt");
    static final Path XREFDATA = TestData.resolve("docs/validation/baseline/XREFFILE/CARDXREF.ksds.txt");

    record Run(Cbstm03a program, BufferedSink stmt, BufferedSink html) {

        List<String> stmtLines() {
            return stmt.records().stream().map(FixedWidthRecord::text).toList();
        }

        List<String> htmlLines() {
            return html.records().stream().map(FixedWidthRecord::text).toList();
        }
    }

    private CreastmtTestSupport() {
    }

    static Cbstm03b files(Supplier<List<FixedWidthRecord>> trnx, Path xref) {
        return new Cbstm03b(KsdsInput.records(Cbstm03b.TRNXFILE, trnx),
                KsdsInput.file(Cbstm03b.XREFFILE, xref, CardXrefRecord.MAPPER.layout(), ASCII),
                KeyedDataset.file(Cbstm03b.CUSTFILE, CUSTDATA, KeyedDataset.Mode.INPUT, CustomerRecord.MAPPER,
                        CustomerRecord::custId, k -> String.format("%09d", k), ASCII),
                KeyedDataset.file(Cbstm03b.ACCTFILE, ACCTDATA, KeyedDataset.Mode.INPUT, AccountRecord.MAPPER,
                        AccountRecord::acctId, k -> String.format("%011d", k), ASCII),
                ASCII);
    }

    static Run program(Supplier<List<FixedWidthRecord>> trnx, Path xref, Sysout sysout) {
        BufferedSink stmt = new BufferedSink(Cbstm03a.STMTFILE);
        BufferedSink html = new BufferedSink(Cbstm03a.HTMLFILE);
        return new Run(new Cbstm03a(files(trnx, xref), stmt, html, ASCII, sysout, "CREASTMT", "STEP040"), stmt,
                html);
    }

    static List<FixedWidthRecord> trnxLines(List<String> lines) {
        return lines.stream().map(l -> FixedWidthRecord.fromLine(Cbstm03a.TRNX_LAYOUT, l, ASCII)).toList();
    }
}
