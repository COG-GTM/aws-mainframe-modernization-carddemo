#!/usr/bin/env python3
"""Apply the golden-set online scenario (scenario.json) to the pristine sample files, independently of the Java code.

CICS online programs cannot run under GnuCOBOL, so the COBOL side of the golden set gets its after-online datasets
from this script: it reads the same sample files the GnuCOBOL baseline loads (app/data/ASCII; USRSEC from
app/data/EBCDIC, converted cp037 like scripts/baseline/baseline.py; TRANSACT starts empty as after `initial-load`)
and replays every write of the scenario from the COBOL source and the rules in docs/modernization/rules/:

  COACTUPC R-39  9600-WRITE-PROCESSING: REWRITE ACCTDAT FROM(ACCT-UPDATE-RECORD), REWRITE CUSTDAT FROM(CUST-UPDATE-RECORD)
  COCRDUPC R-30  9200-WRITE-PROCESSING: REWRITE CARDDAT FROM(CARD-UPDATE-RECORD)
  COTRN02C R-28/R-29  ADD-TRANSACTION: last TRAN-ID + 1, INITIALIZE TRAN-RECORD, WRITE TRANSACT
  COBIL00C R-12/R-13  bill payment: WRITE TRANSACT (type 02, cat 2, full balance), REWRITE ACCTDAT (balance 0)
  COUSR01C R-13  WRITE USRSEC (fields as typed); COUSR02C R-17 REWRITE changed fields; COUSR03C DELETE USRSEC

Field positions and numeric encoding come from the copybooks (layouts.py). The working-storage update records are
laid over the file record byte for byte, exactly as the REWRITE FROM(...) does; INITIALIZE leaves FILLER alone, and
GnuCOBOL starts working storage as spaces, so FILLER bytes of the update records are spaces.

Reads, edits, screens and REST calls are not modelled: the scenario only contains inputs the edits accept (the Java
side proves that by answering 200/201). Output: <out>/<DATASET>.txt (one record per line, the `unload` format) for
ACCTDATA, CUSTDATA, CARDDATA, CARDXREF, TRANSACT, USRSEC, plus <out>/changes.json (every record written).
"""
from __future__ import annotations

import argparse
import json
from decimal import Decimal
from pathlib import Path

from layouts import REPO, Layout, Field, encode, write_records

ASCII = REPO / "app" / "data" / "ASCII"
EBCDIC = REPO / "app" / "data" / "EBCDIC"
SOURCES = {"ACCTDATA": "acctdata.txt", "CUSTDATA": "custdata.txt", "CARDDATA": "carddata.txt",
           "CARDXREF": "cardxref.txt"}

# COACTUPC WORKING-STORAGE 05 ACCT-UPDATE-RECORD (app/cbl/COACTUPC.cbl): unlike CVACT01Y it has no ACCT-ADDR-ZIP,
# so ACCT-UPDATE-GROUP-ID sits where the file record has ACCT-ADDR-ZIP and the FILLER covers ACCT-GROUP-ID.
ACCT_UPDATE_RECORD = [("ACCT-UPDATE-ID", 11, 0, False), ("ACCT-UPDATE-ACTIVE-STATUS", 1, None, None),
                      ("ACCT-UPDATE-CURR-BAL", 12, 2, True), ("ACCT-UPDATE-CREDIT-LIMIT", 12, 2, True),
                      ("ACCT-UPDATE-CASH-CREDIT-LIMIT", 12, 2, True), ("ACCT-UPDATE-OPEN-DATE", 10, None, None),
                      ("ACCT-UPDATE-EXPIRAION-DATE", 10, None, None), ("ACCT-UPDATE-REISSUE-DATE", 10, None, None),
                      ("ACCT-UPDATE-CURR-CYC-CREDIT", 12, 2, True), ("ACCT-UPDATE-CURR-CYC-DEBIT", 12, 2, True),
                      ("ACCT-UPDATE-GROUP-ID", 10, None, None), ("FILLER", 188, None, None)]

# Screen field (Java request property) -> stored field the COACTUPC map shows / ACUP-NEW-* field.
ACCOUNT_SCREEN = {"activeStatus": "ACCT-ACTIVE-STATUS", "creditLimit": "ACCT-CREDIT-LIMIT",
                  "cashCreditLimit": "ACCT-CASH-CREDIT-LIMIT", "currentBalance": "ACCT-CURR-BAL",
                  "currentCycleCredit": "ACCT-CURR-CYC-CREDIT", "currentCycleDebit": "ACCT-CURR-CYC-DEBIT",
                  "groupId": "ACCT-GROUP-ID"}
CUSTOMER_SCREEN = {"firstName": "CUST-FIRST-NAME", "middleName": "CUST-MIDDLE-NAME", "lastName": "CUST-LAST-NAME",
                   "addressLine1": "CUST-ADDR-LINE-1", "addressLine2": "CUST-ADDR-LINE-2",
                   "city": "CUST-ADDR-LINE-3", "state": "CUST-ADDR-STATE-CD", "country": "CUST-ADDR-COUNTRY-CD",
                   "zip": "CUST-ADDR-ZIP", "governmentId": "CUST-GOVT-ISSUED-ID",
                   "eftAccountId": "CUST-EFT-ACCOUNT-ID", "primaryCardHolder": "CUST-PRI-CARD-HOLDER-IND",
                   "ficoScore": "CUST-FICO-CREDIT-SCORE"}
# Lengths of the COACTUPC map input fields that are shorter than the record field (ACSZIPC is 5 bytes).
SCREEN_LENGTH = {"zip": 5}


def numval_c(text: str) -> Decimal:
    """FUNCTION NUMVAL-C: optional sign (leading or trailing, CR/DB), currency sign and commas ignored."""
    t = text.strip().replace(",", "").replace("$", "")
    sign = 1
    for neg in ("-", "CR", "DB"):
        if t.endswith(neg) or t.startswith(neg):
            sign, t = -1, t.replace(neg, "")
    t = t.replace("+", "").strip()
    return sign * Decimal(t or "0")


class Files:
    def __init__(self):
        self.layouts = {ds: Layout(ds) for ds in ("ACCTDATA", "CUSTDATA", "CARDDATA", "CARDXREF", "TRANSACT", "USRSEC")}
        self.data: dict[str, dict[str, str]] = {}
        for ds, name in SOURCES.items():
            self.data[ds] = self._ascii(ds, name)
        raw = (EBCDIC / "AWS.M2.CARDDEMO.USRSEC.PS").read_bytes().decode("cp037")
        lay = self.layouts["USRSEC"]
        self.data["USRSEC"] = {lay.key(r): r for r in (raw[i:i + 80] for i in range(0, len(raw), 80))}
        self.data["TRANSACT"] = {}
        self.log: list[dict] = []

    def _ascii(self, ds: str, name: str) -> dict[str, str]:
        lay = self.layouts[ds]
        out = {}
        for ln in (ASCII / name).read_text(encoding="latin-1").split("\n"):
            ln = ln.rstrip("\r")
            if ln:
                rec = ln[:lay.lrecl].ljust(lay.lrecl)
                out[lay.key(rec)] = rec
        return out

    def read(self, ds: str, key: str) -> str:
        return self.data[ds][key]

    def write(self, ds: str, rec: str, verb: str, rule: str):
        key = self.layouts[ds].key(rec)
        if verb == "WRITE" and key in self.data[ds]:
            raise RuntimeError(f"{ds} {key}: DUPREC")
        if verb == "REWRITE" and key not in self.data[ds]:
            raise RuntimeError(f"{ds} {key}: NOTFND")
        before = self.data[ds].get(key)
        self.data[ds][key] = rec
        self.log.append({"dataset": ds, "verb": verb, "key": key, "rule": rule, "before": before, "after": rec})

    def delete(self, ds: str, key: str, rule: str):
        before = self.data[ds].pop(key)
        self.log.append({"dataset": ds, "verb": "DELETE", "key": key, "rule": rule, "before": before, "after": None})

    def xref_by_account(self, acct_id: str) -> str:
        """CXACAIX: CARDXREF alternate index on XREF-ACCT-ID (first card of the account)."""
        lay = self.layouts["CARDXREF"]
        for key in sorted(self.data["CARDXREF"]):
            rec = self.data["CARDXREF"][key]
            if lay.get(rec, "XREF-ACCT-ID") == acct_id:
                return rec
        raise RuntimeError(f"CXACAIX {acct_id}: NOTFND")

    def last_tran_id(self) -> int:
        """STARTBR HIGH-VALUES + READPREV; ENDFILE -> 0."""
        return int(max(self.data["TRANSACT"])) if self.data["TRANSACT"] else 0


def account_update(f: Files, spec: dict):
    """COACTUPC 9600-WRITE-PROCESSING (R-39). The typed screen is what COACTVWC/COACTUPC display for the stored
    records (9500-STORE-FETCHED-DATA) with the scenario's changes typed over it."""
    acct_l, cust_l = f.layouts["ACCTDATA"], f.layouts["CUSTDATA"]
    acct = f.read("ACCTDATA", spec["acctId"])
    xref = f.xref_by_account(spec["acctId"])
    cust = f.read("CUSTDATA", f.layouts["CARDXREF"].get(xref, "XREF-CUST-ID"))
    unknown = set(spec["set"]) - set(ACCOUNT_SCREEN) - set(CUSTOMER_SCREEN)
    if unknown:
        raise ValueError(f"accountUpdate: unsupported screen fields {sorted(unknown)}")

    def typed(screen: str, stored: str, layout: Layout, rec: str) -> str:
        if screen in spec["set"]:
            return spec["set"][screen]
        f_ = layout.by_name[stored]
        raw = f_.raw(rec)
        if f_.numeric:
            return str(layout_value(f_, raw))
        return raw.rstrip()[:SCREEN_LENGTH.get(screen, f_.length)]

    # ACCT-UPDATE-RECORD
    ws = {"ACCT-UPDATE-ID": acct_l.get(acct, "ACCT-ID"),
          "ACCT-UPDATE-ACTIVE-STATUS": typed("activeStatus", "ACCT-ACTIVE-STATUS", acct_l, acct),
          "ACCT-UPDATE-CURR-BAL": numval_c(typed("currentBalance", "ACCT-CURR-BAL", acct_l, acct)),
          "ACCT-UPDATE-CREDIT-LIMIT": numval_c(typed("creditLimit", "ACCT-CREDIT-LIMIT", acct_l, acct)),
          "ACCT-UPDATE-CASH-CREDIT-LIMIT": numval_c(typed("cashCreditLimit", "ACCT-CASH-CREDIT-LIMIT", acct_l, acct)),
          # STRING year '-' mon '-' day of the split stored dates (not changed by this scenario)
          "ACCT-UPDATE-OPEN-DATE": acct_l.get(acct, "ACCT-OPEN-DATE"),
          "ACCT-UPDATE-EXPIRAION-DATE": acct_l.get(acct, "ACCT-EXPIRAION-DATE"),
          "ACCT-UPDATE-REISSUE-DATE": acct_l.get(acct, "ACCT-REISSUE-DATE"),
          "ACCT-UPDATE-CURR-CYC-CREDIT": numval_c(typed("currentCycleCredit", "ACCT-CURR-CYC-CREDIT", acct_l, acct)),
          "ACCT-UPDATE-CURR-CYC-DEBIT": numval_c(typed("currentCycleDebit", "ACCT-CURR-CYC-DEBIT", acct_l, acct)),
          "ACCT-UPDATE-GROUP-ID": typed("groupId", "ACCT-GROUP-ID", acct_l, acct)}
    out = ""
    for name, length, scale, signed in ACCT_UPDATE_RECORD:
        if name == "FILLER":
            out += " " * length
        elif scale is None:
            out += str(ws[name])[:length].ljust(length)
        else:
            out += encode(Field(name, 0, length, True, signed, scale), ws[name])
    f.write("ACCTDATA", out, "REWRITE", "COACTUPC R-39 (ACCT-UPDATE-RECORD)")

    # CUST-UPDATE-RECORD (same layout as CVCUS01Y)
    rec = cust_l.blank()
    rec = cust_l.put(rec, "CUST-ID", int(cust_l.get(cust, "CUST-ID")))
    for screen, stored in CUSTOMER_SCREEN.items():
        if stored == "CUST-FICO-CREDIT-SCORE":
            rec = cust_l.put(rec, stored, int(typed(screen, stored, cust_l, cust)))
        else:
            rec = cust_l.put(rec, stored, typed(screen, stored, cust_l, cust))
    for n in ("CUST-PHONE-NUM-1", "CUST-PHONE-NUM-2"):   # STRING '(' A ')' B '-' C from the split stored phone
        p = cust_l.get(cust, n)
        rec = cust_l.put(rec, n, f"({p[1:4]}){p[5:8]}-{p[9:13]}")
    rec = cust_l.put(rec, "CUST-SSN", int(cust_l.get(cust, "CUST-SSN")))
    rec = cust_l.put(rec, "CUST-DOB-YYYY-MM-DD", cust_l.get(cust, "CUST-DOB-YYYY-MM-DD"))
    f.write("CUSTDATA", rec, "REWRITE", "COACTUPC R-39 (CUST-UPDATE-RECORD)")


def layout_value(field: Field, raw: str):
    from layouts import decode
    d = decode(field, raw)
    return f"{d:.{field.scale}f}" if field.scale else int(d)


def card_update(f: Files, spec: dict):
    """COCRDUPC 9200-WRITE-PROCESSING (R-30): CARD-UPDATE-RECORD has the CVACT02Y layout; the expiry day is kept."""
    lay = f.layouts["CARDDATA"]
    card = f.read("CARDDATA", spec["cardNum"])
    rec = lay.blank()
    rec = lay.put(rec, "CARD-NUM", spec["cardNum"])
    rec = lay.put(rec, "CARD-ACCT-ID", int(spec["acctId"]))
    rec = lay.put(rec, "CARD-CVV-CD", int(lay.get(card, "CARD-CVV-CD")))
    rec = lay.put(rec, "CARD-EMBOSSED-NAME", spec["embossedName"])
    day = lay.get(card, "CARD-EXPIRAION-DATE")[8:10]
    rec = lay.put(rec, "CARD-EXPIRAION-DATE", f"{spec['expiryYear']}-{spec['expiryMonth'].zfill(2)}-{day}")
    rec = lay.put(rec, "CARD-ACTIVE-STATUS", spec["activeStatus"])
    f.write("CARDDATA", rec, "REWRITE", "COCRDUPC R-30")


def add_transaction(f: Files, t: dict):
    """COTRN02C: R-9/R-11 (card from CXACAIX when the account is typed), R-28 id, R-29 record build."""
    lay, xl = f.layouts["TRANSACT"], f.layouts["CARDXREF"]
    if t["accountId"].strip():
        card = xl.get(f.xref_by_account(str(int(t["accountId"])).zfill(11)), "XREF-CARD-NUM")
    else:
        card = xl.get(f.read("CARDXREF", t["cardNumber"]), "XREF-CARD-NUM")
    rec = lay.blank()
    rec = lay.put(rec, "TRAN-ID", str(f.last_tran_id() + 1).zfill(16))
    rec = lay.put(rec, "TRAN-TYPE-CD", t["typeCode"])
    rec = lay.put(rec, "TRAN-CAT-CD", int(t["categoryCode"]))
    rec = lay.put(rec, "TRAN-SOURCE", t["source"])
    rec = lay.put(rec, "TRAN-DESC", t["description"])
    rec = lay.put(rec, "TRAN-AMT", numval_c(t["amount"]))
    rec = lay.put(rec, "TRAN-MERCHANT-ID", int(t["merchantId"]))
    rec = lay.put(rec, "TRAN-MERCHANT-NAME", t["merchantName"])
    rec = lay.put(rec, "TRAN-MERCHANT-CITY", t["merchantCity"])
    rec = lay.put(rec, "TRAN-MERCHANT-ZIP", t["merchantZip"])
    rec = lay.put(rec, "TRAN-CARD-NUM", card)
    rec = lay.put(rec, "TRAN-ORIG-TS", t["origDate"])
    rec = lay.put(rec, "TRAN-PROC-TS", t["procDate"])
    f.write("TRANSACT", rec, "WRITE", "COTRN02C R-28/R-29")


def bill_payment(f: Files, spec: dict, clock: str):
    """COBIL00C R-12/R-13: pay the whole current balance; WRITE the payment, then REWRITE the account."""
    al, tl, xl = f.layouts["ACCTDATA"], f.layouts["TRANSACT"], f.layouts["CARDXREF"]
    acct = f.read("ACCTDATA", spec["acctId"])
    bal = al.by_name["ACCT-CURR-BAL"]
    from layouts import decode
    amount = decode(bal, bal.raw(acct))
    if amount <= 0:
        raise RuntimeError("COBIL00C R-10: nothing to pay")
    card = xl.get(f.xref_by_account(spec["acctId"]), "XREF-CARD-NUM")
    ts = f"{clock[:10]} {clock[11:19]}.000000"
    rec = tl.blank()
    for name, value in (("TRAN-ID", str(f.last_tran_id() + 1).zfill(16)), ("TRAN-TYPE-CD", "02"),
                        ("TRAN-CAT-CD", 2), ("TRAN-SOURCE", "POS TERM"), ("TRAN-DESC", "BILL PAYMENT - ONLINE"),
                        ("TRAN-AMT", amount), ("TRAN-CARD-NUM", card), ("TRAN-MERCHANT-ID", 999999999),
                        ("TRAN-MERCHANT-NAME", "BILL PAYMENT"), ("TRAN-MERCHANT-CITY", "N/A"),
                        ("TRAN-MERCHANT-ZIP", "N/A"), ("TRAN-ORIG-TS", ts), ("TRAN-PROC-TS", ts)):
        rec = tl.put(rec, name, value)
    f.write("TRANSACT", rec, "WRITE", "COBIL00C R-12 (payment transaction)")
    f.write("ACCTDATA", al.put(acct, "ACCT-CURR-BAL", amount - amount), "REWRITE", "COBIL00C R-12 (balance 0)")


def user_add(f: Files, u: dict):
    """COUSR01C R-13: SEC-USER-DATA from the five fields as typed; SEC-USR-FILLER is working storage (spaces)."""
    lay = f.layouts["USRSEC"]
    rec = lay.blank()
    for name, key in (("SEC-USR-ID", "userId"), ("SEC-USR-FNAME", "firstName"), ("SEC-USR-LNAME", "lastName"),
                      ("SEC-USR-PWD", "password"), ("SEC-USR-TYPE", "userType")):
        rec = lay.put(rec, name, u[key])
    f.write("USRSEC", rec, "WRITE", "COUSR01C R-13")


def user_update(f: Files, u: dict):
    """COUSR02C: READ UPDATE, MOVE each typed field that differs, REWRITE (the rest of the record is kept)."""
    lay = f.layouts["USRSEC"]
    rec = f.read("USRSEC", u["userId"].ljust(8))
    for name, key in (("SEC-USR-FNAME", "firstName"), ("SEC-USR-LNAME", "lastName"), ("SEC-USR-PWD", "password"),
                      ("SEC-USR-TYPE", "userType")):
        if key in u:
            rec = lay.put(rec, name, u[key])
    f.write("USRSEC", rec, "REWRITE", "COUSR02C R-17")


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--scenario", type=Path, default=Path(__file__).with_name("scenario.json"))
    ap.add_argument("--out", type=Path, required=True)
    a = ap.parse_args()
    s = json.loads(a.scenario.read_text())
    f = Files()
    account_update(f, s["accountUpdate"])
    card_update(f, s["cardUpdate"])
    for t in s["transactions"]:
        add_transaction(f, t)
    bill_payment(f, s["billPayment"], s["clock"])
    user_add(f, s["userAdd"])
    user_update(f, s["userUpdate"])
    f.delete("USRSEC", s["userDelete"].ljust(8), "COUSR03C DELETE")
    for ds, recs in f.data.items():
        write_records(a.out / f"{ds}.txt", [recs[k] for k in sorted(recs, key=lambda k: k.encode("latin-1"))])
    (a.out / "changes.json").write_text(json.dumps(f.log, indent=1) + "\n")
    print(f"apply_online_scenario: {len(f.log)} records written -> {a.out}")
    for e in f.log:
        print(f"  {e['verb']:7} {e['dataset']:8} {e['key'].strip():16} {e['rule']}")


if __name__ == "__main__":
    main()
