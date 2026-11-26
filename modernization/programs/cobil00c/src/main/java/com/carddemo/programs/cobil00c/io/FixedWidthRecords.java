package com.carddemo.programs.cobil00c.io;

import com.carddemo.programs.cobil00c.copybook.AccountRecord;
import com.carddemo.programs.cobil00c.copybook.CardXrefRecord;
import com.carddemo.programs.cobil00c.copybook.Cobol;
import com.carddemo.programs.cobil00c.copybook.TranRecord;
import com.carddemo.programs.cobil00c.copybook.ZonedDecimal;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Function;

/**
 * Fixed-width (copybook byte offset) codecs for the CardDemo ASCII datasets used by COBIL00C:
 * ACCTDAT (CVACT01Y), CARDXREF (CVACT03Y) and TRANSACT (CVTRA05Y). Signed numerics are zoned
 * decimal with an overpunched sign in the last byte.
 */
public final class FixedWidthRecords {

    private FixedWidthRecords() {
    }

    public static <T> List<T> readAll(Path file, Function<String, T> decoder) throws IOException {
        return Files.readAllLines(file, StandardCharsets.UTF_8).stream()
                .filter(l -> !l.isBlank()).map(decoder).toList();
    }

    private static String slice(String line, int start, int len) {
        String padded = Cobol.alnum(line, Math.max(line.length(), start + len));
        return padded.substring(start, start + len);
    }

    // ---- CVACT01Y ACCOUNT-RECORD
    public static AccountRecord decodeAccount(String line) {
        return new AccountRecord(
                Long.parseLong(slice(line, 0, 11)),
                slice(line, 11, 1),
                ZonedDecimal.decode(slice(line, 12, 12), 2),
                ZonedDecimal.decode(slice(line, 24, 12), 2),
                ZonedDecimal.decode(slice(line, 36, 12), 2),
                slice(line, 48, 10),
                slice(line, 58, 10),
                slice(line, 68, 10),
                ZonedDecimal.decode(slice(line, 78, 12), 2),
                ZonedDecimal.decode(slice(line, 90, 12), 2),
                slice(line, 102, 10),
                slice(line, 112, 10));
    }

    public static String encodeAccount(AccountRecord r) {
        String s = r.key() + r.acctActiveStatus()
                + ZonedDecimal.encode(r.acctCurrBal(), 12, 2)
                + ZonedDecimal.encode(r.acctCreditLimit(), 12, 2)
                + ZonedDecimal.encode(r.acctCashCreditLimit(), 12, 2)
                + r.acctOpenDate() + r.acctExpiraionDate() + r.acctReissueDate()
                + ZonedDecimal.encode(r.acctCurrCycCredit(), 12, 2)
                + ZonedDecimal.encode(r.acctCurrCycDebit(), 12, 2)
                + r.acctAddrZip() + r.acctGroupId();
        return Cobol.alnum(s, AccountRecord.RECORD_LENGTH);
    }

    // ---- CVACT03Y CARD-XREF-RECORD (the ASCII extract omits the trailing FILLER)
    public static CardXrefRecord decodeCardXref(String line) {
        return new CardXrefRecord(slice(line, 0, 16), Long.parseLong(slice(line, 16, 9)),
                Long.parseLong(slice(line, 25, 11)));
    }

    public static String encodeCardXref(CardXrefRecord r) {
        return Cobol.alnum(r.xrefCardNum() + Cobol.unsignedDigits(r.xrefCustId(), 9) + r.acctKey(),
                CardXrefRecord.RECORD_LENGTH);
    }

    // ---- CVTRA05Y TRAN-RECORD
    public static TranRecord decodeTran(String line) {
        return new TranRecord(
                slice(line, 0, 16),
                slice(line, 16, 2),
                Integer.parseInt(slice(line, 18, 4)),
                slice(line, 22, 10),
                slice(line, 32, 100),
                ZonedDecimal.decode(slice(line, 132, 11), 2),
                Long.parseLong(slice(line, 143, 9)),
                slice(line, 152, 50),
                slice(line, 202, 50),
                slice(line, 252, 10),
                slice(line, 262, 16),
                slice(line, 278, 26),
                slice(line, 304, 26));
    }

    public static String encodeTran(TranRecord r) {
        String s = r.tranId() + r.tranTypeCd() + Cobol.unsignedDigits(r.tranCatCd(), 4) + r.tranSource()
                + r.tranDesc() + ZonedDecimal.encode(r.tranAmt(), 11, 2)
                + Cobol.unsignedDigits(r.tranMerchantId(), 9) + r.tranMerchantName() + r.tranMerchantCity()
                + r.tranMerchantZip() + r.tranCardNum() + r.tranOrigTs() + r.tranProcTs();
        return Cobol.alnum(s, TranRecord.RECORD_LENGTH);
    }
}
