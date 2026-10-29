package com.carddemo.transactiontype.domain;

import static com.carddemo.transactiontype.domain.LegacyRuleException.Kind.CONFIRMATION_REQUIRED;
import static com.carddemo.transactiontype.domain.LegacyRuleException.Kind.CONFLICT;
import static com.carddemo.transactiontype.domain.LegacyRuleException.Kind.DB_ERROR;
import static com.carddemo.transactiontype.domain.LegacyRuleException.Kind.LOCKED;
import static com.carddemo.transactiontype.domain.LegacyRuleException.Kind.NOT_FOUND;
import static com.carddemo.transactiontype.domain.LegacyRuleException.Kind.NO_CHANGE;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.springframework.stereotype.Service;

/** Online maintenance: COTRTLIC (list / inline update / delete) and COTRTUPC (get / add / update / delete). */
@Service
public class TransactionTypeService {

    public static final int DEFAULT_PAGE_SIZE = 7;

    private final TransactionTypeStore store;
    private final int pageSize;

    public TransactionTypeService(TransactionTypeStore store) {
        this.store = store;
        this.pageSize = DEFAULT_PAGE_SIZE;
    }

    /** BR-TT-010..017. */
    public TransactionTypePage list(String typeCodeFilter, String descriptionFilter, String startKey,
                                    PageDirection direction) {
        ListFilter filter = new ListFilter(
                TransactionTypeRules.parseTypeCodeFilter(typeCodeFilter),
                TransactionTypeRules.descriptionFilterPattern(descriptionFilter));
        long total = sqlGuard("Error reading TRANSACTION_TYPE table ", () -> store.count(filter));
        String key = startKey == null || startKey.isBlank() ? "" : startKey;
        PageDirection dir = direction == null ? PageDirection.FIRST : direction;

        if (dir == PageDirection.PREVIOUS) {
            List<TransactionType> back = sqlGuard("Error on fetch Cursor C-TR-TYPE-BACKWARD",
                    () -> store.findBackward(key, filter, pageSize));
            if (back.isEmpty()) {
                return forwardPage(key, filter, total, "No previous pages to display");
            }
            List<TransactionType> rows = new ArrayList<>(back);
            Collections.reverse(rows);
            return forwardPage(rows.get(0).code(), filter, total, null);
        }
        TransactionTypePage page = forwardPage(dir == PageDirection.FIRST ? "" : key, filter, total, null);
        if (dir == PageDirection.NEXT && !page.hasNextPage() && page.message() == null) {
            return withMessage(page, "No more pages to display");
        }
        return page;
    }

    private TransactionTypePage forwardPage(String startKey, ListFilter filter, long total, String message) {
        List<TransactionType> rows = sqlGuard("C-TR-TYPE-FORWARD fetch",
                () -> store.findForward(startKey, filter, pageSize + 1));
        boolean hasNext = rows.size() > pageSize;
        List<TransactionType> items = hasNext ? List.copyOf(rows.subList(0, pageSize)) : List.copyOf(rows);
        String first = items.isEmpty() ? null : items.get(0).code();
        String next = hasNext ? rows.get(pageSize).code() : null;
        boolean hasPrevious = first != null && !sqlGuard("Error on fetch Cursor C-TR-TYPE-BACKWARD",
                () -> store.findBackward(first, filter, 1)).isEmpty();
        String msg = message;
        if (items.isEmpty() && msg == null) {
            msg = "No records found for this search condition.";
        }
        return new TransactionTypePage(items, first, next, hasNext, hasPrevious, total, msg);
    }

    private static TransactionTypePage withMessage(TransactionTypePage p, String message) {
        return new TransactionTypePage(p.items(), p.firstKey(), p.nextStartKey(), p.hasNextPage(),
                p.hasPreviousPage(), p.totalMatching(), message);
    }

    /** BR-TT-020. */
    public TransactionType get(String rawCode) {
        String code = TransactionTypeRules.normalizeTypeCode(rawCode);
        return sqlGuard("Error accessing: TRANSACTION_TYPE table.", () -> store.findByCode(code))
                .orElseThrow(() -> new LegacyRuleException("BR-TT-020", NOT_FOUND,
                        "No record found for this key in database", SqlCodes.NOT_FOUND));
    }

    /** BR-TT-030 (COTRTUPC PF5 on a key that was not found). */
    public MaintenanceResult create(String rawCode, String rawDescription) {
        String code = TransactionTypeRules.normalizeTypeCode(rawCode);
        String description = TransactionTypeRules.validateDescription(rawDescription);
        insert(new TransactionType(code, description));
        return new MaintenanceResult(new TransactionType(code, description), "Changes committed to database", true);
    }

    /** BR-TT-008, BR-TT-040..042 (COTRTLIC 9200-UPDATE-RECORD, COTRTUPC 9600-WRITE-PROCESSING). */
    public MaintenanceResult update(String rawCode, String rawDescription, boolean createIfMissing) {
        String code = TransactionTypeRules.normalizeTypeCode(rawCode);
        var existing = sqlGuard("Error accessing: TRANSACTION_TYPE table.", () -> store.findByCode(code));
        if (existing.isEmpty() && !createIfMissing) {
            throw new LegacyRuleException("BR-TT-020", NOT_FOUND,
                    "No record found for this key in database", SqlCodes.NOT_FOUND);
        }
        if (existing.isPresent() && TransactionTypeRules.isUnchanged(existing.get().description(), rawDescription)) {
            throw new LegacyRuleException("BR-TT-008", NO_CHANGE,
                    "No change detected with respect to database values.");
        }
        String description = TransactionTypeRules.validateDescription(rawDescription);
        int updated;
        try {
            updated = store.updateDescription(code, description);
        } catch (SqlCodeException e) {
            if (e.sqlCode() == SqlCodes.DEADLOCK_OR_TIMEOUT) {
                throw new LegacyRuleException("BR-TT-041", LOCKED, "Deadlock. Someone else updating ?", e.sqlCode());
            }
            throw dbError("BR-TT-060", "Update failed with", e);
        }
        if (updated == 0) {
            if (!createIfMissing) {
                throw new LegacyRuleException("BR-TT-040", NOT_FOUND,
                        "Record not found. Deleted by others ? ", SqlCodes.NOT_FOUND);
            }
            insert(new TransactionType(code, description));
            return new MaintenanceResult(new TransactionType(code, description), "Changes committed to database", true);
        }
        return new MaintenanceResult(new TransactionType(code, description), "Changes committed to database", false);
    }

    /** BR-TT-050..052 (COTRTUPC PF4 twice, COTRTLIC 'D' + F10). */
    public MaintenanceResult delete(String rawCode, boolean confirmed) {
        TransactionType current = get(rawCode);
        if (!confirmed) {
            throw new LegacyRuleException("BR-TT-050", CONFIRMATION_REQUIRED,
                    "Delete this record ? Press F4 to confirm (resend with confirm=true)");
        }
        int deleted;
        try {
            deleted = store.delete(current.code());
        } catch (SqlCodeException e) {
            if (e.sqlCode() == SqlCodes.DELETE_RESTRICTED_BY_FK) {
                throw new LegacyRuleException("BR-TT-051", CONFLICT,
                        "Please delete associated child records first:SQLCODE :" + e.sqlCode(), e.sqlCode());
            }
            throw dbError("BR-TT-060", "Delete failed with message:", e);
        }
        if (deleted == 0) {
            throw new LegacyRuleException("BR-TT-052", NOT_FOUND,
                    "Record not found. Deleted by others ? ", SqlCodes.NOT_FOUND);
        }
        return new MaintenanceResult(current, "Delete successful.", false);
    }

    public List<TransactionTypeCategory> categories(String rawCode) {
        String code = get(rawCode).code();
        return sqlGuard("Error accessing: TRANSACTION_TYPE_CATEGORY table.", () -> store.findCategories(code));
    }

    /** BR-TT-080. */
    public List<String> extractTransactionTypes() {
        return sqlGuard("Error accessing: TRANSACTION_TYPE table.", store::extractTransactionTypes);
    }

    /** BR-TT-081. */
    public List<String> extractCategories() {
        return sqlGuard("Error accessing: TRANSACTION_TYPE_CATEGORY table.", store::extractCategories);
    }

    private void insert(TransactionType tt) {
        try {
            store.insert(tt);
        } catch (SqlCodeException e) {
            if (e.sqlCode() == SqlCodes.DUPLICATE_KEY) {
                throw new LegacyRuleException("BR-TT-030", CONFLICT,
                        "Error inserting record into: TRANSACTION_TYPE Table. SQLCODE:" + e.sqlCode()
                                + ": transaction type " + tt.code() + " already exists", e.sqlCode());
            }
            throw dbError("BR-TT-060", "Error inserting record into: TRANSACTION_TYPE Table.", e);
        }
    }

    private static LegacyRuleException dbError(String ruleId, String action, SqlCodeException e) {
        return new LegacyRuleException(ruleId, DB_ERROR, action + " SQLCODE:" + e.sqlCode(), e.sqlCode());
    }

    private static <T> T sqlGuard(String action, java.util.function.Supplier<T> call) {
        try {
            return call.get();
        } catch (SqlCodeException e) {
            throw dbError("BR-TT-060", action, e);
        }
    }
}
