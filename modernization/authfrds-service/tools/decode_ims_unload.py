#!/usr/bin/env python3
"""Decode the legacy IMS unload (DBPAUTP0, EBCDIC cp037) into JSON seed data for the IMS stub.

Layouts: PAUTSUM0 = cpy/CIPAUSMY.cpy, PAUTDTL1 = cpy/CIPAUDTY.cpy. Each segment is preceded by its
8-byte segment name plus an unload prefix; the payload offsets below were found by inspecting the file.
Usage: tools/decode_ims_unload.py <DBPAUTP0.dat> <out.json>
"""
import json
import sys

SUM_PAYLOAD = 29   # bytes from start of "PAUTSUM0" to PA-ACCT-ID
DTL_PAYLOAD = 29   # bytes from start of "PAUTDTL1" to PA-AUTH-DATE-9C


def packed(b, scale=0):
    digits = "".join(f"{x:02x}" for x in b)
    sign = -1 if digits[-1] in "bd" else 1
    v = int(digits[:-1]) * sign
    return v / (10 ** scale) if scale else v


def text(b):
    return b.decode("cp037")


def detail(d, o):
    f = {}
    def take(name, n, kind="x", scale=0):
        nonlocal o
        raw = d[o:o + n]
        o += n
        f[name] = packed(raw, scale) if kind == "p" else text(raw)
    take("authDate9c", 3, "p")
    take("authTime9c", 5, "p")
    for name, n in [("authOrigDate", 6), ("authOrigTime", 6), ("cardNum", 16), ("authType", 4),
                    ("cardExpiryDate", 4), ("messageType", 6), ("messageSource", 6), ("authIdCode", 6),
                    ("authRespCode", 2), ("authRespReason", 4), ("processingCode", 6)]:
        take(name, n)
    take("transactionAmt", 7, "p", 2)
    take("approvedAmt", 7, "p", 2)
    for name, n in [("merchantCategoryCode", 4), ("acqrCountryCode", 3), ("posEntryMode", 2),
                    ("merchantId", 15), ("merchantName", 22), ("merchantCity", 13), ("merchantState", 2),
                    ("merchantZip", 9), ("transactionId", 15), ("matchStatus", 1), ("authFraud", 1),
                    ("fraudRptDate", 8)]:
        take(name, n)
    return f


def main(src, out):
    d = open(src, "rb").read()
    accounts, current = [], None
    i = 0
    while True:
        s, t = d.find(b"\xd7\xc1\xe4\xe3\xe2\xe4\xd4\xf0", i), d.find(b"\xd7\xc1\xe4\xe3\xc4\xe3\xd3\xf1", i)
        if s < 0 and t < 0:
            break
        if s >= 0 and (t < 0 or s < t):
            o = s + SUM_PAYLOAD
            acct, cust = packed(d[o:o + 6]), text(d[o + 6:o + 15])
            if acct > 0:
                current = {"acctId": acct, "custId": int(cust), "authorizations": []}
                accounts.append(current)
            else:
                current = None
            i = s + 8
        else:
            if current is not None:
                current["authorizations"].append(detail(d, t + DTL_PAYLOAD))
            i = t + 8
    json.dump(accounts, open(out, "w"), indent=1)
    print(f"accounts={len(accounts)} details={sum(len(a['authorizations']) for a in accounts)}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
