package com.carddemo.interest.model;

import com.carddemo.interest.CobolDecimal;
import java.math.BigDecimal;

/**
 * Copybook CVTRA05Y — transaction record (RECLN 350), as written by 1300-B-WRITE-TX.
 *
 * <p>Every field keeps the width of its PICTURE so that a fixed-length writer can emit a
 * byte-identical 350-character record.
 */
public record TransactionRecord(
        String tranId,
        String typeCd,
        String categoryCd,
        String source,
        String description,
        BigDecimal amount,
        long merchantId,
        String merchantName,
        String merchantCity,
        String merchantZip,
        String cardNumber,
        String originTimestamp,
        String processTimestamp) {

    public TransactionRecord {
        tranId = CobolDecimal.alphanumeric(tranId, 16);
        typeCd = CobolDecimal.alphanumeric(typeCd, 2);
        categoryCd = CobolDecimal.zoned(categoryCd, 4);
        source = CobolDecimal.alphanumeric(source, 10);
        description = CobolDecimal.alphanumeric(description, 100);
        amount = CobolDecimal.toAmount(amount);
        merchantName = CobolDecimal.alphanumeric(merchantName, 50);
        merchantCity = CobolDecimal.alphanumeric(merchantCity, 50);
        merchantZip = CobolDecimal.alphanumeric(merchantZip, 10);
        cardNumber = CobolDecimal.alphanumeric(cardNumber, 16);
        originTimestamp = CobolDecimal.alphanumeric(originTimestamp, 26);
        processTimestamp = CobolDecimal.alphanumeric(processTimestamp, 26);
    }

    /** The 350-byte fixed-length image written to the TRANSACT file. */
    public String toFixedLengthRecord() {
        StringBuilder sb = new StringBuilder(350);
        sb.append(tranId).append(typeCd).append(categoryCd).append(source).append(description)
          .append(CobolDecimal.signedZoned(amount, 9)).append(CobolDecimal.zoned(merchantId, 9))
          .append(merchantName).append(merchantCity)
          .append(merchantZip).append(cardNumber).append(originTimestamp).append(processTimestamp)
          .append(" ".repeat(20));
        return sb.toString();
    }
}
