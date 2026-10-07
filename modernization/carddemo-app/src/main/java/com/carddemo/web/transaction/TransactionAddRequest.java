package com.carddemo.web.transaction;

import com.carddemo.transaction.online.TransactionForm;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Size;

/** COTRN2A input fields (lengths from the BMS map) plus {@code CONFIRM} and the PF5 copy-last switch. */
@Schema(description = "Transaction add (COTRN02C / map COTRN2A). Account wins over card when both are given.")
public record TransactionAddRequest(
        @Schema(description = "ACTIDIN, up to 11 digits", example = "00000000001") @Size(max = 11) String accountId,
        @Schema(description = "CARDNIN, up to 16 digits (used when accountId is blank)", example = "")
        @Size(max = 16) String cardNumber,
        @Schema(description = "TTYPCD, up to 2 digits (TRANTYPE key)", example = "01") @Size(max = 2) String typeCode,
        @Schema(description = "TCATCD, up to 4 digits (TRANCATG key with the type)", example = "0001")
        @Size(max = 4) String categoryCode,
        @Schema(description = "TRNSRC", example = "POS TERM") @Size(max = 10) String source,
        @Schema(description = "TDESC", example = "Online purchase") @Size(max = 60) String description,
        @Schema(description = "TRNAMT, exactly [+-]99999999.99", example = "-00000012.34") @Size(max = 12)
        String amount,
        @Schema(description = "TORIGDT YYYY-MM-DD", example = "2022-07-06") @Size(max = 10) String origDate,
        @Schema(description = "TPROCDT YYYY-MM-DD", example = "2022-07-06") @Size(max = 10) String procDate,
        @Schema(description = "MID, up to 9 digits", example = "000000001") @Size(max = 9) String merchantId,
        @Schema(description = "MNAME", example = "Corner Store") @Size(max = 30) String merchantName,
        @Schema(description = "MCITY", example = "Seattle") @Size(max = 25) String merchantCity,
        @Schema(description = "MZIP", example = "98101") @Size(max = 10) String merchantZip,
        @Schema(description = "CONFIRM: Y = add, N or blank = validate only", example = "N") @Size(max = 1)
        String confirm,
        @Schema(description = "PF5: copy the last transaction's data into the form (keys still required)",
                example = "false") Boolean copyLast) {

    public TransactionForm toForm() {
        return new TransactionForm(accountId, cardNumber, typeCode, categoryCode, source, description, amount,
                origDate, procDate, merchantId, merchantName, merchantCity, merchantZip);
    }

    public boolean copyLastRequested() {
        return Boolean.TRUE.equals(copyLast);
    }

    /** R-30: every field cleared after a successful WRITE. */
    static TransactionAddRequest cleared() {
        return new TransactionAddRequest("", "", "", "", "", "", "", "", "", "", "", "", "", "", false);
    }

    static TransactionAddRequest of(TransactionForm f, String confirm) {
        return new TransactionAddRequest(f.accountId(), f.cardNumber(), f.typeCode(), f.categoryCode(), f.source(),
                f.description(), f.amount(), f.origDate(), f.procDate(), f.merchantId(), f.merchantName(),
                f.merchantCity(), f.merchantZip(), confirm, false);
    }
}
