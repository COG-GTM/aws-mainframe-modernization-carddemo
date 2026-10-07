package com.carddemo.batch.exchange;

import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.Field;
import com.carddemo.common.codec.RecordLayout;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

/**
 * The five {@code EXPORT-REC-TYPE} values of {@code CVEXPORT}, in the order CBEXPORT writes them, each with its
 * source dataset, the CBIMPORT output it splits into, and the field-for-field {@code MOVE}s of
 * {@code CBEXPORT 2x00-CREATE-*-EXP-REC} / {@code CBIMPORT 23x0-PROCESS-*-RECORD} (the same pairs both ways).
 */
public enum ExportRecordType {

    CUSTOMER("C", Dataset.CUSTDATA, "CUSTOUT", "CUSTDATA.IMPORT",
            "CUST-ID", "CUST-FIRST-NAME", "CUST-MIDDLE-NAME", "CUST-LAST-NAME",
            "CUST-ADDR-LINE-1=EXP-CUST-ADDR-LINE(1)", "CUST-ADDR-LINE-2=EXP-CUST-ADDR-LINE(2)",
            "CUST-ADDR-LINE-3=EXP-CUST-ADDR-LINE(3)", "CUST-ADDR-STATE-CD", "CUST-ADDR-COUNTRY-CD", "CUST-ADDR-ZIP",
            "CUST-PHONE-NUM-1=EXP-CUST-PHONE-NUM(1)", "CUST-PHONE-NUM-2=EXP-CUST-PHONE-NUM(2)", "CUST-SSN",
            "CUST-GOVT-ISSUED-ID", "CUST-DOB-YYYY-MM-DD", "CUST-EFT-ACCOUNT-ID", "CUST-PRI-CARD-HOLDER-IND",
            "CUST-FICO-CREDIT-SCORE"),
    ACCOUNT("A", Dataset.ACCTDATA, "ACCTOUT", "ACCTDATA.IMPORT",
            "ACCT-ID", "ACCT-ACTIVE-STATUS", "ACCT-CURR-BAL", "ACCT-CREDIT-LIMIT", "ACCT-CASH-CREDIT-LIMIT",
            "ACCT-OPEN-DATE", "ACCT-EXPIRAION-DATE", "ACCT-REISSUE-DATE", "ACCT-CURR-CYC-CREDIT",
            "ACCT-CURR-CYC-DEBIT", "ACCT-ADDR-ZIP", "ACCT-GROUP-ID"),
    CARD_XREF("X", Dataset.CARDXREF, "XREFOUT", "CARDXREF.IMPORT",
            "XREF-CARD-NUM", "XREF-CUST-ID", "XREF-ACCT-ID"),
    TRANSACTION("T", Dataset.TRANSACT, "TRNXOUT", "TRANSACT.IMPORT",
            "TRAN-ID", "TRAN-TYPE-CD", "TRAN-CAT-CD", "TRAN-SOURCE", "TRAN-DESC", "TRAN-AMT", "TRAN-MERCHANT-ID",
            "TRAN-MERCHANT-NAME", "TRAN-MERCHANT-CITY", "TRAN-MERCHANT-ZIP", "TRAN-CARD-NUM", "TRAN-ORIG-TS",
            "TRAN-PROC-TS"),
    CARD("D", Dataset.CARDDATA, "CARDOUT", "CARDDATA.IMPORT",
            "CARD-NUM", "CARD-ACCT-ID", "CARD-CVV-CD", "CARD-EMBOSSED-NAME", "CARD-EXPIRAION-DATE",
            "CARD-ACTIVE-STATUS");

    /** A {@code MOVE} between a field of the dataset record and its {@code EXP-} twin in {@code CVEXPORT}. */
    public record Move(Field datasetField, Field exportField) {
    }

    private final String code;
    private final Dataset dataset;
    private final String ddname;
    private final String importGdgBase;
    private final List<String> moveSpecs;
    private List<Move> moves;

    ExportRecordType(String code, Dataset dataset, String ddname, String importGdgBase, String... moveSpecs) {
        this.code = code;
        this.dataset = dataset;
        this.ddname = ddname;
        this.importGdgBase = importGdgBase;
        this.moveSpecs = Arrays.asList(moveSpecs);
    }

    public String code() {
        return code;
    }

    public Dataset dataset() {
        return dataset;
    }

    /** The CBIMPORT output DD this type is written to. */
    public String ddname() {
        return ddname;
    }

    /** GDG base of the CBIMPORT output ({@code AWS.M2.CARDDEMO.<base>} in {@code CBIMPORT.jcl}). */
    public String importGdgBase() {
        return importGdgBase;
    }

    public RecordLayout datasetLayout() {
        return dataset.mapper().layout();
    }

    public synchronized List<Move> moves() {
        if (moves == null) {
            RecordLayout source = datasetLayout();
            RecordLayout export = ExportRecordCodec.LAYOUT;
            List<Move> resolved = new ArrayList<>(moveSpecs.size());
            for (String spec : moveSpecs) {
                int eq = spec.indexOf('=');
                String datasetName = eq < 0 ? spec : spec.substring(0, eq);
                String exportName = eq < 0 ? "EXP-" + spec : spec.substring(eq + 1);
                resolved.add(new Move(source.field(datasetName), exportField(export, exportName)));
            }
            moves = List.copyOf(resolved);
        }
        return moves;
    }

    public static Optional<ExportRecordType> ofCode(String code) {
        return Arrays.stream(values()).filter(t -> t.code.equals(code)).findFirst();
    }

    private static Field exportField(RecordLayout export, String name) {
        int paren = name.indexOf('(');
        if (paren < 0) {
            return export.field(name);
        }
        int subscript = Integer.parseInt(name.substring(paren + 1, name.length() - 1));
        return export.field(name.substring(0, paren)).subscript(subscript);
    }
}
