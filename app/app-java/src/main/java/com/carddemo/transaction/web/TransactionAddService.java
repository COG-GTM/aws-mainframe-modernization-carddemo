package com.carddemo.transaction.web;

import com.carddemo.transaction.cobol.Pic;
import com.carddemo.transaction.domain.CardXref;
import com.carddemo.transaction.domain.Transaction;
import com.carddemo.transaction.repository.CardXrefRepository;
import com.carddemo.transaction.repository.TransactionRepository;
import com.carddemo.transaction.web.TransactionDtos.AddTransactionRequest;
import com.carddemo.transaction.web.TransactionDtos.AddTransactionResponse;
import org.springframework.dao.DataAccessException;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.format.ResolverStyle;

/**
 * COTRN02C (add transaction). Validation order and messages follow the COBOL program; as on the CICS
 * screen, the first failing check ends the request. Map field widths (COTRN2A) are applied to every
 * input before validation.
 */
@Service
public class TransactionAddService {

    private static final DateTimeFormatter ISO_STRICT = DateTimeFormatter.ofPattern("uuuu-MM-dd")
            .withResolverStyle(ResolverStyle.STRICT);

    private final CardXrefRepository xrefs;
    private final TransactionRepository transactions;

    public TransactionAddService(CardXrefRepository xrefs, TransactionRepository transactions) {
        this.xrefs = xrefs;
        this.transactions = transactions;
    }

    /** PROCESS-ENTER-KEY. */
    @Transactional
    public synchronized AddTransactionResponse processEnterKey(AddTransactionRequest in) {
        CardXref xref = validateInputKeyFields(in);
        String acct = Pic.zeroFill(xref.getAcctId(), 11);
        String card = xref.getCardNum().stripTrailing();
        validateInputDataFields(in, acct, card);
        String confirm = field(in.confirm(), 1);
        switch (confirm) {
            case "Y", "y" -> {
                return addTransaction(in, xref);
            }
            case "N", "n", " ", "\u0000" ->
                    throw new OnlineException(HttpStatus.BAD_REQUEST, "Confirm to add this transaction...", acct, card);
            default ->
                    throw new OnlineException(HttpStatus.BAD_REQUEST, "Invalid value. Valid values are (Y/N)...", acct, card);
        }
    }

    /** VALIDATE-INPUT-KEY-FIELDS with READ-CXACAIX-FILE / READ-CCXREF-FILE. */
    CardXref validateInputKeyFields(AddTransactionRequest in) {
        String acctIn = field(in.accountId(), 11);
        String cardIn = field(in.cardNumber(), 16);
        if (!Pic.isBlank(acctIn)) {
            String v = acctIn.stripTrailing();
            if (!Pic.isNumeric(v)) {
                throw new OnlineException(HttpStatus.BAD_REQUEST, "Account ID must be Numeric...");
            }
            long acctId = Long.parseLong(v);
            return read(() -> xrefs.findFirstByAcctIdOrderByCardNumAsc(acctId),
                    "Unable to lookup Acct in XREF AIX file...")
                    .orElseThrow(() -> new OnlineException(HttpStatus.NOT_FOUND, "Account ID NOT found..."));
        }
        if (!Pic.isBlank(cardIn)) {
            String v = cardIn.stripTrailing();
            if (!Pic.isNumeric(v)) {
                throw new OnlineException(HttpStatus.BAD_REQUEST, "Card Number must be Numeric...");
            }
            String cardNum = Pic.zeroFill(Long.parseLong(v), 16);
            return read(() -> xrefs.findById(cardNum), "Unable to lookup Card # in XREF file...")
                    .orElseThrow(() -> new OnlineException(HttpStatus.NOT_FOUND, "Card Number NOT found..."));
        }
        throw new OnlineException(HttpStatus.BAD_REQUEST, "Account or Card Number must be entered...");
    }

    /** VALIDATE-INPUT-DATA-FIELDS. */
    void validateInputDataFields(AddTransactionRequest in, String acct, String card) {
        String type = field(in.typeCd(), 2);
        String cat = field(in.categoryCd(), 4);
        String amount = field(in.amount(), 12);
        String orig = field(in.origDate(), 10);
        String proc = field(in.procDate(), 10);
        String mid = field(in.merchantId(), 9);

        requireNotBlank(type, "Type CD can NOT be empty...", acct, card);
        requireNotBlank(field(in.categoryCd(), 4), "Category CD can NOT be empty...", acct, card);
        requireNotBlank(field(in.source(), 10), "Source can NOT be empty...", acct, card);
        requireNotBlank(field(in.description(), 60), "Description can NOT be empty...", acct, card);
        requireNotBlank(amount, "Amount can NOT be empty...", acct, card);
        requireNotBlank(orig, "Orig Date can NOT be empty...", acct, card);
        requireNotBlank(proc, "Proc Date can NOT be empty...", acct, card);
        requireNotBlank(mid, "Merchant ID can NOT be empty...", acct, card);
        requireNotBlank(field(in.merchantName(), 30), "Merchant Name can NOT be empty...", acct, card);
        requireNotBlank(field(in.merchantCity(), 25), "Merchant City can NOT be empty...", acct, card);
        requireNotBlank(field(in.merchantZip(), 10), "Merchant Zip can NOT be empty...", acct, card);

        if (!Pic.isNumeric(type.stripTrailing())) {
            throw error("Type CD must be Numeric...", acct, card);
        }
        if (!Pic.isNumeric(cat.stripTrailing())) {
            throw error("Category CD must be Numeric...", acct, card);
        }
        if ((amount.charAt(0) != '-' && amount.charAt(0) != '+')
                || !Pic.isNumeric(amount.substring(1, 9))
                || amount.charAt(9) != '.'
                || !Pic.isNumeric(amount.substring(10, 12))) {
            throw error("Amount should be in format -99999999.99", acct, card);
        }
        if (!isDateFormat(orig)) {
            throw error("Orig Date should be in format YYYY-MM-DD", acct, card);
        }
        if (!isDateFormat(proc)) {
            throw error("Proc Date should be in format YYYY-MM-DD", acct, card);
        }
        if (!isValidDate(orig)) {                                   // CALL 'CSUTLDTC'
            throw error("Orig Date - Not a valid date...", acct, card);
        }
        if (!isValidDate(proc)) {
            throw error("Proc Date - Not a valid date...", acct, card);
        }
        if (!Pic.isNumeric(mid.stripTrailing())) {
            throw error("Merchant ID must be Numeric...", acct, card);
        }
    }

    /** ADD-TRANSACTION + WRITE-TRANSACT-FILE. */
    AddTransactionResponse addTransaction(AddTransactionRequest in, CardXref xref) {
        String acct = Pic.zeroFill(xref.getAcctId(), 11);
        String card = xref.getCardNum().stripTrailing();
        String lastId = read(transactions::findTopByOrderByTranIdDesc, "Unable to lookup Transaction...")
                .map(Transaction::getTranId)
                .orElse("0");                                       // READPREV ENDFILE -> MOVE ZEROS TO TRAN-ID
        String last = lastId.stripTrailing();
        if (!Pic.isNumeric(last)) {
            throw new OnlineException(HttpStatus.INTERNAL_SERVER_ERROR, "Unable to lookup Transaction...", acct, card);
        }
        String tranId = Pic.zeroFill(Long.parseLong(last) + 1, 16);

        Transaction t = new Transaction();                          // INITIALIZE TRAN-RECORD
        t.setTranId(tranId);
        t.setTypeCd(field(in.typeCd(), 2));
        t.setCatCd(Integer.parseInt(field(in.categoryCd(), 4).stripTrailing()));
        t.setSource(field(in.source(), 10));
        t.setDesc(field(in.description(), 60));
        t.setAmt(new BigDecimal(field(in.amount(), 12)));         // NUMVAL-C
        t.setCardNum(xref.getCardNum());
        t.setMerchantId(Long.parseLong(field(in.merchantId(), 9).stripTrailing()));
        t.setMerchantName(field(in.merchantName(), 30));
        t.setMerchantCity(field(in.merchantCity(), 25));
        t.setMerchantZip(field(in.merchantZip(), 10));
        t.setOrigTs(field(in.origDate(), 10));
        t.setProcTs(field(in.procDate(), 10));

        if (read(() -> transactions.existsById(tranId), "Unable to Add Transaction...")) {
            throw new OnlineException(HttpStatus.CONFLICT, "Tran ID already exist...", acct, card);
        }
        read(() -> transactions.save(t), "Unable to Add Transaction...");
        return new AddTransactionResponse(tranId, acct, card,
                "Transaction added successfully.  Your Tran ID is " + tranId + ".");
    }

    private static String field(String value, int width) {
        return Pic.x(value, width);
    }

    private static void requireNotBlank(String value, String message, String acct, String card) {
        if (Pic.isBlank(value)) {
            throw error(message, acct, card);
        }
    }

    private static OnlineException error(String message, String acct, String card) {
        return new OnlineException(HttpStatus.BAD_REQUEST, message, acct, card);
    }

    private static boolean isDateFormat(String d) {
        return Pic.isNumeric(d.substring(0, 4)) && d.charAt(4) == '-' && Pic.isNumeric(d.substring(5, 7))
                && d.charAt(7) == '-' && Pic.isNumeric(d.substring(8, 10));
    }

    private static boolean isValidDate(String d) {
        try {
            LocalDate.parse(d, ISO_STRICT);
            return true;
        } catch (DateTimeParseException e) {
            return false;
        }
    }

    private static <T> T read(java.util.function.Supplier<T> op, String message) {
        try {
            return op.get();
        } catch (DataAccessException e) {
            throw new OnlineException(HttpStatus.INTERNAL_SERVER_ERROR, message);
        }
    }

}
