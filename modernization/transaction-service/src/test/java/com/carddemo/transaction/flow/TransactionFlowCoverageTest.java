package com.carddemo.transaction.flow;

import static java.util.Map.entry;

import com.carddemo.testsupport.flow.CobolMessages;
import com.carddemo.testsupport.flow.FlowCoverage;
import java.nio.file.Path;
import java.util.Map;
import org.junit.jupiter.api.Test;

/** Gate g-online-tests for transaction-service, and the COBOL message texts its flow tests assert. */
class TransactionFlowCoverageTest {

    private static final Path SOURCE = CobolMessages.source(TransactionFlowTest.class);
    private static final String IO_ERROR = "CICS RESP other than NORMAL/NOTFND/DUPREC: an unhandled exception, 500";
    private static final String CONFIRM = "Y/N confirmation step; the POST itself is the confirmation (api-style.md §1)";
    private static final String PAGE_EDGE = "page-edge text; the API answers an empty page or 400 on page < 0 (api-style.md §4)";
    private static final String TRAN_ID_SEQUENCE =
            "TRAN-ID comes from a database sequence under an advisory lock, so a duplicate key cannot occur";
    private static final String COPY_LAST = "COPY-LAST-TRAN-DATA's READPREV text; the client reads the last list row";

    @Test
    void everyTransactionServiceScreenKeyHasAFlowTest() {
        FlowCoverage.assertEveryScreenKeyCovered("transaction-service", TransactionFlowTest.class);
    }

    @Test
    void COTRN00C_to_COTRN02C_messagesAreAsserted() {
        CobolMessages.assertAsserted("COTRN00C", Map.ofEntries(
                entry("Invalid selection. Valid value is S", "row selection S is client navigation to GET /transactions/{id}"),
                entry("Unable to lookup transaction...", IO_ERROR),
                entry("You are already at the bottom of the page...", PAGE_EDGE),
                entry("You are already at the top of the page...", PAGE_EDGE),
                entry("You are at the top of the page...", PAGE_EDGE),
                entry("You have reached the bottom of the page...", PAGE_EDGE),
                entry("You have reached the top of the page...", PAGE_EDGE)), SOURCE);
        CobolMessages.assertAsserted("COTRN01C", Map.of(
                "Tran ID can NOT be empty...", "the id is a path segment; GET /api/v1/transactions/ is the list",
                "Unable to lookup Transaction...", IO_ERROR), SOURCE);
        CobolMessages.assertAsserted("COTRN02C", Map.ofEntries(
                entry("Confirm to add this transaction...", CONFIRM),
                entry("Invalid value. Valid values are (Y/N)...", CONFIRM),
                entry("Tran ID already exist...", TRAN_ID_SEQUENCE),
                entry("Transaction ID NOT found...", COPY_LAST),
                entry("Unable to Add Transaction...", IO_ERROR),
                entry("Unable to lookup Acct in XREF AIX file...", IO_ERROR),
                entry("Unable to lookup Card # in XREF file...", IO_ERROR),
                entry("Unable to lookup Transaction...", IO_ERROR)), SOURCE);
    }

    @Test
    void COBIL00C_messagesAreAsserted() {
        CobolMessages.assertAsserted("COBIL00C", Map.ofEntries(
                entry("Confirm to make a bill payment...", CONFIRM),
                entry("Invalid value. Valid values are (Y/N)...", CONFIRM),
                entry("Tran ID already exist...", TRAN_ID_SEQUENCE),
                entry("Transaction ID NOT found...", "READPREV of the last TRAN-ID; the id comes from a sequence"),
                entry("Unable to Add Bill pay Transaction...", IO_ERROR),
                entry("Unable to Update Account...", IO_ERROR),
                entry("Unable to lookup Account...", IO_ERROR),
                entry("Unable to lookup Transaction...", IO_ERROR),
                entry("Unable to lookup XREF AIX file...", IO_ERROR)), SOURCE);
    }
}
