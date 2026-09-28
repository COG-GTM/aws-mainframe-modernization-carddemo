"""Fixed-width parsers for the GnuCOBOL output files (copybooks CVACT01Y, CVTRA01Y, CVTRA05Y, CBTRN02C trailer)."""

from __future__ import annotations

from decimal import Decimal

# Trailing sign overpunch as written by GnuCOBOL with -fsign=EBCDIC (and in the app/data/ASCII fixtures).
_POSITIVE = {c: str(i) for i, c in enumerate("{ABCDEFGHI")}
_NEGATIVE = {c: str(i) for i, c in enumerate("}JKLMNOPQR")}


def zoned(field: str, scale: int) -> Decimal:
    digits, last = field[:-1], field[-1]
    if last in _NEGATIVE:
        value = Decimal(digits + _NEGATIVE[last]).copy_negate()
    elif last in _POSITIVE:
        value = Decimal(digits + _POSITIVE[last])
    else:
        value = Decimal(field)
    return value.scaleb(-scale)


def _text(field: str) -> str | None:
    v = field.rstrip()
    return v or None


def account(line: str) -> tuple:
    """CVACT01Y ACCOUNT-RECORD (300)."""
    return (int(line[0:11]), line[11], zoned(line[12:24], 2), zoned(line[24:36], 2), zoned(line[36:48], 2),
            _text(line[48:58]), _text(line[58:68]), _text(line[68:78]), zoned(line[78:90], 2),
            zoned(line[90:102], 2), _text(line[102:112]), _text(line[112:122]))


ACCOUNT_SQL = ("SELECT acct_id, active_status, curr_bal, credit_limit, cash_credit_limit, open_date::text, "
               "expiration_date::text, reissue_date::text, curr_cyc_credit, curr_cyc_debit, addr_zip, group_id "
               "FROM account ORDER BY acct_id")
ACCOUNT_FIELDS = ["acct_id", "active_status", "curr_bal", "credit_limit", "cash_credit_limit", "open_date",
                  "expiration_date", "reissue_date", "curr_cyc_credit", "curr_cyc_debit", "addr_zip", "group_id"]


def tcatbal(line: str) -> tuple:
    """CVTRA01Y TRAN-CAT-BAL-RECORD (50): key acct/type/cat + balance."""
    return int(line[0:11]), line[11:13], int(line[13:17]), zoned(line[17:28], 2)


TCATBAL_SQL = ('SELECT acct_id, type_cd, cat_cd, balance FROM tran_cat_balance '
               'ORDER BY acct_id, type_cd COLLATE "C", cat_cd')


def transaction(line: str) -> tuple:
    """CVTRA05Y TRAN-RECORD (350) without TRAN-PROC-TS (FUNCTION CURRENT-DATE, masked in the golden files)."""
    return (line[0:16], line[16:18], int(line[18:22]), _text(line[22:32]), _text(line[32:132]),
            zoned(line[132:143], 2), int(line[143:152]), _text(line[152:202]), _text(line[202:252]),
            _text(line[252:262]), line[262:278], line[278:304].rstrip())


TRANSACTION_SQL = ("SELECT tran_id, type_cd, cat_cd, source, description, amt, merchant_id, merchant_name, "
                   "merchant_city, merchant_zip, card_num, to_char(orig_ts, 'YYYY-MM-DD HH24:MI:SS.US') "
                   "FROM transaction {where} ORDER BY tran_id")


def reject(line: str) -> tuple[str, int, str]:
    """DALYREJS: 350-byte DALYTRAN record + WS-VALIDATION-TRAILER (FAIL-REASON 9(4), FAIL-REASON-DESC X(76))."""
    return line[0:16], int(line[350:354]), line[354:430].rstrip()


def lines(text: str) -> list[str]:
    return [line for line in text.splitlines() if line]
