package com.carddemo.transaction;

import com.carddemo.common.ApiException;
import com.carddemo.common.ErrorCode;
import com.carddemo.common.Money;
import com.carddemo.common.PageQuery;
import com.carddemo.common.PageResponse;
import com.carddemo.common.Text;
import com.carddemo.common.ValidationErrors;
import com.carddemo.transaction.TransactionDtos.CreateTransactionRequest;
import com.carddemo.transaction.TransactionDtos.CreateTransactionResponse;
import com.carddemo.transaction.TransactionDtos.TransactionDetail;
import com.carddemo.transaction.TransactionDtos.TransactionSummary;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.format.ResolverStyle;
import java.util.List;
import java.util.regex.Pattern;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** COTRN00C (list), COTRN01C (detail), COTRN02C (add). */
@Service
public class TransactionService {

    static final int DEFAULT_PAGE_SIZE = 10;
    static final String ADD_PROGRAM = "COTRN02C";

    private static final Pattern AMOUNT = Pattern.compile("^[+-]?[0-9]{1,8}\\.[0-9]{2}$");
    private static final Pattern ISO_DATE = Pattern.compile("^[0-9]{4}-[0-9]{2}-[0-9]{2}$");
    private static final DateTimeFormatter STRICT_DATE = DateTimeFormatter.ofPattern("uuuu-MM-dd")
            .withResolverStyle(ResolverStyle.STRICT);

    private final TransactionRepository transactions;
    private final TranIdAllocator allocator;
    private final JdbcClient jdbc;

    public TransactionService(TransactionRepository transactions, TranIdAllocator allocator, JdbcClient jdbc) {
        this.transactions = transactions;
        this.allocator = allocator;
        this.jdbc = jdbc;
    }

    @Transactional(readOnly = true)
    public PageResponse<TransactionSummary> list(String startKey, String direction, Integer pageSize) {
        if (!Text.isBlank(startKey) && !Text.isDigits(startKey.strip())) {
            throw ApiException.validation("COTRN00C", "startKey", "Tran ID must be Numeric ...");
        }
        String key = Text.isBlank(startKey) ? null : Text.leftPadZeros(startKey.strip(), 16);
        PageQuery query = PageQuery.of(key, direction, pageSize, DEFAULT_PAGE_SIZE, "COTRN00C");
        PageResponse<TransactionRecord> page = transactions.page(query);
        List<TransactionSummary> items = page.items().stream()
                .map(t -> new TransactionSummary(t.tranId(),
                        t.origTs() == null ? "" : t.origTs().toLocalDate().toString(), t.description(),
                        Money.format(t.amt())))
                .toList();
        String message = null;
        if (items.isEmpty() && query.startKey() != null) {
            message = query.direction() == PageQuery.Direction.NEXT
                    ? "You have reached the bottom of the page..."
                    : "You have reached the top of the page...";
        }
        return new PageResponse<>(items, page.firstKey(), page.lastKey(), page.hasNext(), page.hasPrev(), message);
    }

    @Transactional(readOnly = true)
    public TransactionDetail detail(String tranIdIn) {
        if (Text.isBlank(tranIdIn)) {
            throw ApiException.validation("COTRN01C", "tranId", "Tran ID can NOT be empty...");
        }
        String tranId = tranIdIn.strip();
        if (Text.isDigits(tranId)) {
            tranId = Text.leftPadZeros(tranId, 16);
        }
        TransactionRecord t = transactions.findById(tranId)
                .orElseThrow(() -> ApiException.notFound("COTRN01C", "Transaction ID NOT found..."));
        return new TransactionDetail(t.tranId(), t.cardNum(), t.typeCd(), t.catCd(), t.source(), t.description(),
                Money.format(t.amt()), t.origTs(), t.procTs(), t.merchantId(), t.merchantName(), t.merchantCity(),
                t.merchantZip());
    }

    @Transactional
    public CreateTransactionResponse create(CreateTransactionRequest r) {
        if (r == null) {
            throw ApiException.validation(ADD_PROGRAM, "acctId", "Account or Card Number must be entered...");
        }
        String cardNum = resolveCard(r);
        ValidationErrors errors = new ValidationErrors(ADD_PROGRAM);
        requireText(errors, "typeCd", r.typeCd(), "Type CD can NOT be empty...");
        requireText(errors, "catCd", r.catCd(), "Category CD can NOT be empty...");
        requireText(errors, "source", r.source(), "Source can NOT be empty...");
        requireText(errors, "description", r.description(), "Description can NOT be empty...");
        requireText(errors, "amt", r.amt(), "Amount can NOT be empty...");
        requireText(errors, "origDate", r.origDate(), "Orig Date can NOT be empty...");
        requireText(errors, "procDate", r.procDate(), "Proc Date can NOT be empty...");
        requireText(errors, "merchantId", r.merchantId(), "Merchant ID can NOT be empty...");
        requireText(errors, "merchantName", r.merchantName(), "Merchant Name can NOT be empty...");
        requireText(errors, "merchantCity", r.merchantCity(), "Merchant City can NOT be empty...");
        requireText(errors, "merchantZip", r.merchantZip(), "Merchant Zip can NOT be empty...");
        errors.throwIfAny();

        String typeCd = r.typeCd().strip();
        String catCd = r.catCd().strip();
        if (!Text.isDigits(typeCd) || typeCd.length() > 2) {
            errors.add("typeCd", "Type CD must be Numeric...");
        } else if (!Text.isDigits(catCd) || catCd.length() > 4) {
            errors.add("catCd", "Category CD must be Numeric...");
        }
        errors.throwIfAny();
        String amt = r.amt().strip();
        if (!AMOUNT.matcher(amt).matches()) {
            errors.add("amt", "Amount should be in format -99999999.99");
        }
        errors.throwIfAny();
        if (!ISO_DATE.matcher(r.origDate().strip()).matches()) {
            errors.add("origDate", "Orig Date should be in format YYYY-MM-DD");
        }
        errors.throwIfAny();
        if (!ISO_DATE.matcher(r.procDate().strip()).matches()) {
            errors.add("procDate", "Proc Date should be in format YYYY-MM-DD");
        }
        errors.throwIfAny();
        LocalDate origDate = parseDate(r.origDate().strip());
        if (origDate == null) {
            errors.add("origDate", "Orig Date - Not a valid date...");
        }
        errors.throwIfAny();
        LocalDate procDate = parseDate(r.procDate().strip());
        if (procDate == null) {
            errors.add("procDate", "Proc Date - Not a valid date...");
        }
        errors.throwIfAny();
        String merchantId = r.merchantId().strip();
        if (!Text.isDigits(merchantId) || merchantId.length() > 9) {
            errors.add("merchantId", "Merchant ID must be Numeric...");
        }
        errors.throwIfAny();
        checkLength(errors, "source", r.source(), 10);
        checkLength(errors, "description", r.description(), 100);
        checkLength(errors, "merchantName", r.merchantName(), 50);
        checkLength(errors, "merchantCity", r.merchantCity(), 50);
        checkLength(errors, "merchantZip", r.merchantZip(), 10);
        errors.throwIfAny();

        String type = Text.leftPadZeros(typeCd, 2);
        int cat = Integer.parseInt(catCd);
        if (!transactions.categoryExists(type, cat)) {
            throw ApiException.validation(ADD_PROGRAM, "catCd", "Type CD / Category CD combination not found...");
        }

        String tranId = allocator.next();
        TransactionRecord record = new TransactionRecord(tranId, type, cat, r.source().strip(),
                r.description().strip(), new BigDecimal(amt), Integer.valueOf(merchantId), r.merchantName().strip(),
                r.merchantCity().strip(), r.merchantZip().strip(), cardNum, origDate.atStartOfDay(),
                procDate.atStartOfDay());
        try {
            transactions.insert(record);
        } catch (DuplicateKeyException ex) {
            throw new ApiException(ErrorCode.DUPLICATE, "Tran ID already exist...", ADD_PROGRAM);
        }
        return new CreateTransactionResponse(tranId,
                "Transaction added successfully. Your Tran ID is " + tranId + ".");
    }

    /**
     * VALIDATE-INPUT-KEY-FIELDS: exactly one key is accepted; an account is resolved to its card through the
     * CXACAIX alternate index, a card number is validated against CCXREF.
     */
    private String resolveCard(CreateTransactionRequest r) {
        if (!Text.isBlank(r.acctId()) && !Text.isBlank(r.cardNum())) {
            throw ApiException.validation(ADD_PROGRAM, "cardNum", "Enter either Account ID or Card Number, not both");
        }
        if (!Text.isBlank(r.acctId())) {
            String acct = r.acctId().strip();
            if (!Text.isDigits(acct) || acct.length() > 11) {
                throw ApiException.validation(ADD_PROGRAM, "acctId", "Account ID must be Numeric...");
            }
            return jdbc.sql("SELECT card_num FROM card_xref WHERE acct_id = :id ORDER BY card_num COLLATE \"C\" "
                    + "LIMIT 1")
                    .param("id", Long.parseLong(acct)).query(String.class).optional()
                    .orElseThrow(() -> ApiException.notFound(ADD_PROGRAM, "Account ID NOT found..."));
        }
        if (!Text.isBlank(r.cardNum())) {
            String card = r.cardNum().strip();
            if (!Text.isDigits(card) || card.length() > 16) {
                throw ApiException.validation(ADD_PROGRAM, "cardNum", "Card Number must be Numeric...");
            }
            String padded = Text.leftPadZeros(card, 16);
            return jdbc.sql("SELECT card_num FROM card_xref WHERE card_num = :id").param("id", padded)
                    .query(String.class).optional()
                    .orElseThrow(() -> ApiException.notFound(ADD_PROGRAM, "Card Number NOT found..."));
        }
        throw ApiException.validation(ADD_PROGRAM, "acctId", "Account or Card Number must be entered...");
    }

    private static void requireText(ValidationErrors errors, String field, String value, String message) {
        if (!errors.hasErrors() && Text.isBlank(value)) {
            errors.add(field, message);
        }
    }

    private static void checkLength(ValidationErrors errors, String field, String value, int max) {
        if (value.strip().length() > max) {
            errors.add(field, field + " can not be longer than " + max + " characters...");
        }
    }

    private static LocalDate parseDate(String value) {
        try {
            return LocalDate.parse(value, STRICT_DATE);
        } catch (DateTimeParseException ex) {
            return null;
        }
    }
}
