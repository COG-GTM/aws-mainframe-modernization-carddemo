package com.carddemo.transaction.web;

import com.carddemo.transaction.cobol.Pic;
import com.carddemo.transaction.domain.Transaction;
import com.fasterxml.jackson.annotation.JsonInclude;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

/**
 * Request/response bodies standing in for the BMS maps COTRN0A, COTRN1A and COTRN2A.
 * PIC X values are returned right-trimmed; the stored values keep their fixed-length padding.
 */
public final class TransactionDtos {

    private TransactionDtos() {
    }

    /** One list row of COTRN0A (TRNIDnn / TDATEnn / TDESCnn / TAMTnnn). */
    public record TransactionRow(String tranId, String date, String description, String amount) {
        static TransactionRow of(Transaction t) {
            String ts = t.getOrigTs();
            String date = ts.substring(5, 7) + "/" + ts.substring(8, 10) + "/" + ts.substring(2, 4);
            return new TransactionRow(t.getTranId().stripTrailing(), date,
                    Pic.x(t.getDesc(), 26).stripTrailing(), editedAmount(t.getAmt()));
        }
    }

    /** COTRN0A screen: up to 10 rows plus the paging state kept in CDEMO-CT00-INFO. */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public record TransactionPage(List<TransactionRow> transactions, String firstTranId, String lastTranId,
                                  boolean nextPage, String message) {
    }

    /** COTRN1A screen. */
    public record TransactionDetail(String tranId, String cardNumber, String typeCd, int catCd, String source,
                                    BigDecimal amount, String description, String origTs, String procTs,
                                    long merchantId, String merchantName, String merchantCity, String merchantZip) {
        static TransactionDetail of(Transaction t) {
            return new TransactionDetail(t.getTranId().stripTrailing(), t.getCardNum().stripTrailing(),
                    t.getTypeCd().stripTrailing(), t.getCatCd(), t.getSource().stripTrailing(), t.getAmt(),
                    t.getDesc().stripTrailing(), t.getOrigTs().stripTrailing(), t.getProcTs().stripTrailing(),
                    t.getMerchantId(), t.getMerchantName().stripTrailing(), t.getMerchantCity().stripTrailing(),
                    t.getMerchantZip().stripTrailing());
        }
    }

    /** COTRN2A input fields (all alphanumeric on the map; widths are applied server side). */
    public record AddTransactionRequest(String accountId, String cardNumber, String typeCd, String categoryCd,
                                        String source, String description, String amount, String origDate,
                                        String procDate, String merchantId, String merchantName,
                                        String merchantCity, String merchantZip, String confirm) {
    }

    public record AddTransactionResponse(String tranId, String accountId, String cardNumber, String message) {
    }

    /** {@code PIC +99999999.99}. */
    public static String editedAmount(BigDecimal amount) {
        BigDecimal v = Pic.s9(amount, 8, 2);
        String digits = v.abs().setScale(2, RoundingMode.UNNECESSARY).toPlainString();
        return (v.signum() < 0 ? "-" : "+") + "0".repeat(11 - digits.length()) + digits;
    }
}
