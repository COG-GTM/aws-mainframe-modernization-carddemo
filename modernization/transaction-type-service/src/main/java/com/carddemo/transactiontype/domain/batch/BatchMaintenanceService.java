package com.carddemo.transactiontype.domain.batch;

import com.carddemo.transactiontype.domain.SqlCodeException;
import com.carddemo.transactiontype.domain.TransactionType;
import com.carddemo.transactiontype.domain.TransactionTypeStore;
import java.util.ArrayList;
import java.util.List;
import org.springframework.stereotype.Service;

/**
 * COBTUPDT batch maintenance (BR-TT-070..076). Input records are 53 bytes: action (1), TR_TYPE (2),
 * TR_DESCRIPTION (50). Like the COBOL 9999-ABEND paragraph, an error sets return code 4 and processing
 * continues with the next record; earlier changes stay committed.
 */
@Service
public class BatchMaintenanceService {

    private static final int RECORD_LENGTH = 53;
    private final TransactionTypeStore store;

    public BatchMaintenanceService(TransactionTypeStore store) {
        this.store = store;
    }

    public BatchResult run(List<String> records) {
        List<String> out = new ArrayList<>();
        int rc = 0, inserted = 0, updated = 0, deleted = 0, ignored = 0, errors = 0;
        out.add("OPEN FILE OK");
        for (String raw : records) {
            String rec = pad(raw);
            out.add(("PROCESSING   " + rec).stripTrailing());
            char action = rec.charAt(0);
            String code = rec.substring(1, 3);
            String description = rec.substring(3, RECORD_LENGTH).stripTrailing();
            try {
                switch (action) {
                    case 'A' -> {
                        out.add("ADDING RECORD");
                        store.insert(new TransactionType(code, description));
                        out.add("RECORD INSERTED SUCCESSFULLY");
                        inserted++;
                    }
                    case 'U' -> {
                        out.add("UPDATING RECORD");
                        if (store.updateDescription(code, description) == 0) {
                            rc = abend(out, "No records found.");
                            errors++;
                        } else {
                            out.add("RECORD UPDATED SUCCESSFULLY");
                            updated++;
                        }
                    }
                    case 'D' -> {
                        out.add("DELETING RECORD");
                        if (store.delete(code) == 0) {
                            rc = abend(out, "No records found.");
                            errors++;
                        } else {
                            out.add("RECORD DELETED SUCCESSFULLY");
                            deleted++;
                        }
                    }
                    case '*' -> {
                        out.add("IGNORING COMMENTED LINE");
                        ignored++;
                    }
                    default -> {
                        rc = abend(out, "ERROR: TYPE NOT VALID");
                        errors++;
                    }
                }
            } catch (SqlCodeException e) {
                rc = abend(out, "Error accessing: TRANSACTION_TYPE table. SQLCODE:" + e.sqlCode());
                errors++;
            }
        }
        return new BatchResult(rc, records.size(), inserted, updated, deleted, ignored, errors, out);
    }

    private static int abend(List<String> out, String message) {
        out.add(message);
        return 4;
    }

    private static String pad(String raw) {
        String r = raw == null ? "" : raw;
        return r.length() >= RECORD_LENGTH ? r.substring(0, RECORD_LENGTH)
                : r + " ".repeat(RECORD_LENGTH - r.length());
    }
}
