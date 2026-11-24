package com.carddemo.transaction;

import com.carddemo.common.domain.CardRepository;
import com.carddemo.common.domain.CardTransaction;
import com.carddemo.common.domain.CardTransactionRepository;
import com.carddemo.common.domain.CardXref;
import com.carddemo.common.domain.CardXrefRepository;
import com.carddemo.common.domain.TransactionIdGenerator;
import com.carddemo.common.error.ApiException;
import com.carddemo.common.web.KeysetBrowser;
import com.carddemo.common.web.KeysetPage;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeParseException;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** COTRN00C (CT00 list), COTRN01C (CT01 view), COTRN02C (CT02 add). */
@Service
public class TransactionService {

    static final int PAGE_SIZE = 10;

    private final CardTransactionRepository transactions;
    private final CardXrefRepository xrefs;
    private final CardRepository cards;
    private final TransactionIdGenerator ids;

    public TransactionService(CardTransactionRepository transactions, CardXrefRepository xrefs,
                              CardRepository cards, TransactionIdGenerator ids) {
        this.transactions = transactions;
        this.xrefs = xrefs;
        this.cards = cards;
        this.ids = ids;
    }

    @Transactional(readOnly = true)
    public KeysetPage<TransactionView> list(String startKey, String after, String before, int page) {
        if (startKey != null && !startKey.isBlank() && !startKey.trim().matches("\\d{1,16}")) {
            throw ApiException.badRequest("TRAN_ID_NUMERIC", "Tran ID must be Numeric ...");
        }
        String start = startKey == null || startKey.isBlank() ? null : leftPad16(startKey.trim());
        return KeysetBrowser.browse(PAGE_SIZE, page, start, after, before,
                (from, n) -> transactions.findByTranIdGreaterThanEqualOrderByTranIdAsc(from, PageRequest.of(0, n))
                        .stream().map(TransactionView::of).toList(),
                (from, n) -> transactions.findByTranIdLessThanOrderByTranIdDesc(from, PageRequest.of(0, n))
                        .stream().map(TransactionView::of).toList(),
                TransactionView::tranId);
    }

    @Transactional(readOnly = true)
    public TransactionView view(String tranId) {
        if (tranId == null || tranId.isBlank()) {
            throw ApiException.badRequest("TRAN_ID_REQUIRED", "Tran ID can NOT be empty...");
        }
        return transactions.findById(tranId.trim()).map(TransactionView::of).orElseThrow(() ->
                ApiException.notFound("TRAN_NOT_FOUND", "Transaction ID NOT found..."));
    }

    @Transactional
    public OperationResult add(AddTransactionRequest r) {
        String cardNum = resolveCard(r);
        require(r.typeCd(), "Type CD");
        require(r.catCd(), "Category CD");
        require(r.source(), "Source");
        require(r.description(), "Description");
        require(r.amount(), "Amount");
        require(r.origDate(), "Orig Date");
        require(r.procDate(), "Proc Date");
        require(r.merchantId(), "Merchant ID");
        require(r.merchantName(), "Merchant Name");
        require(r.merchantCity(), "Merchant City");
        require(r.merchantZip(), "Merchant Zip");
        if (!r.typeCd().trim().matches("\\d{1,2}")) {
            fail("Type CD must be Numeric...");
        }
        if (!r.catCd().trim().matches("\\d{1,4}")) {
            fail("Category CD must be Numeric...");
        }
        if (!r.amount().trim().matches("[+-]\\d{8}\\.\\d{2}")) {
            fail("Amount should be in format -99999999.99");
        }
        LocalDate orig = date(r.origDate(), "Orig Date");
        LocalDate proc = date(r.procDate(), "Proc Date");
        if (!r.merchantId().trim().matches("\\d{1,9}")) {
            fail("Merchant ID must be Numeric...");
        }
        String confirm = r.confirm() == null ? "" : r.confirm().trim().toUpperCase();
        if (confirm.isEmpty() || confirm.equals("N")) {
            return new OperationResult("Confirm to add this transaction...", true, null, null);
        }
        if (!confirm.equals("Y")) {
            fail("Invalid value. Valid values are (Y/N)...");
        }
        CardTransaction t = new CardTransaction();
        t.setTranId(ids.next());
        t.setTypeCd(String.format("%02d", Integer.parseInt(r.typeCd().trim())));
        t.setCatCd(Integer.parseInt(r.catCd().trim()));
        t.setSource(r.source().trim());
        t.setDescription(r.description().trim());
        t.setAmount(new BigDecimal(r.amount().trim()));
        t.setMerchantId(Long.parseLong(r.merchantId().trim()));
        t.setMerchantName(r.merchantName().trim());
        t.setMerchantCity(r.merchantCity().trim());
        t.setMerchantZip(r.merchantZip().trim());
        t.setCardNum(cardNum);
        t.setOrigTs(orig.atStartOfDay());
        t.setProcTs(proc.atStartOfDay());
        transactions.save(t);
        return new OperationResult("Transaction added successfully.  Your Tran ID is " + t.getTranId() + ".",
                false, t.getTranId(), null);
    }

    /** Key fields: account (resolved through CXACAIX) or card (through CCXREF). */
    private String resolveCard(AddTransactionRequest r) {
        if (r.acctId() != null && !r.acctId().isBlank()) {
            if (!r.acctId().trim().matches("\\d{1,11}")) {
                fail("Account ID must be Numeric...");
            }
            CardXref x = xrefs.findFirstByAcctIdOrderByCardNumAsc(Long.parseLong(r.acctId().trim()))
                    .orElseThrow(() -> ApiException.notFound("ACCOUNT_NOT_FOUND", "Account ID NOT found..."));
            return x.getCardNum();
        }
        if (r.cardNum() != null && !r.cardNum().isBlank()) {
            if (!r.cardNum().trim().matches("\\d{1,16}")) {
                fail("Card Number must be Numeric...");
            }
            String card = leftPad16(r.cardNum().trim());
            if (!xrefs.existsById(card)) {
                throw ApiException.notFound("CARD_NOT_FOUND", "Card Number NOT found...");
            }
            return card;
        }
        fail("Account or Card Number must be entered...");
        return null;
    }

    private static LocalDate date(String value, String label) {
        String v = value.trim();
        if (!v.matches("\\d{4}-\\d{2}-\\d{2}")) {
            fail(label + " should be in format YYYY-MM-DD");
        }
        try {
            return LocalDate.parse(v);
        } catch (DateTimeParseException e) {
            fail(label + " - Not a valid date...");
            return null;
        }
    }

    private static void require(String v, String label) {
        if (v == null || v.isBlank()) {
            fail(label + " can NOT be empty...");
        }
    }

    static void fail(String message) {
        throw ApiException.badRequest("VALIDATION", message);
    }

    private static String leftPad16(String digits) {
        return "0".repeat(16 - digits.length()) + digits;
    }
}
