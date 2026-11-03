#!/usr/bin/env python3
"""Diffs the Java outputs against the GnuCOBOL golden set, file for file and field for field.

usage: compare.py <cobol-dir> <java-dir> <report-dir>

<cobol-dir> is what reference.py wrote, <java-dir> what java.sh wrote. For every job a
<report-dir>/<JOB>/reconciliation.md lists each file (records, bytes identical or not), each field
difference, and what it is: a mask or exclusion listed in ACCEPTED (with its reason) or an
unexplained difference. Exit status 1 when any difference is unexplained or a file is missing.

Record layouts are read from the phase-3 copybook layouts in common/.../record/layout/*.java (the
elementary fields in order, checked against the declared record length), so the field names are the
copybook names. Numeric fields compare by value (zoned decimal, either overpunch convention); alpha
fields byte for byte.
"""
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[1]
LAYOUTS = HERE.parent / "common/src/main/java/com/carddemo/common/record/layout"
DATA = REPO / "app/data/ASCII"

FIELD = re.compile(r'B\.(alpha|numeric)\("([^"]+)",\s*"([^"]+)"\)')


def pic_len(pic):
    n = 0
    for m in re.finditer(r"([X9])(?:\((\d+)\))?", pic.replace("S", "").replace("V", "")):
        n += int(m.group(2) or 1)
    return n


def layout(java_name):
    text = (LAYOUTS / f"{java_name}.java").read_text()
    fields, pos = [], 0
    for kind, name, pic in FIELD.findall(text):
        n = pic_len(pic)
        fields.append((name, pos, n, kind == "numeric" and "V" in pic or kind == "numeric", pic))
        pos += n
    declared = int(re.search(r"B\.build\((\d+)\)", text).group(1))
    if declared != pos:
        raise SystemExit(f"{java_name}: fields add up to {pos}, layout declares {declared}")
    return declared, fields


def raw_layout(lrecl):
    return lrecl, [("RECORD", 0, lrecl, False, f"X({lrecl})")]


POS = {c: i for i, c in enumerate("{ABCDEFGHI")}
NEG = {**{c: i for i, c in enumerate("}JKLMNOPQR")}, **{c: i for i, c in enumerate("pqrstuvwxy")}}


def zoned(text, pic):
    if not text:
        return text
    last, sign = text[-1], 1
    if last in POS:
        last = str(POS[last])
    elif last in NEG:
        last, sign = str(NEG[last]), -1
    digits = text[:-1] + last
    if not digits.isdigit():
        return text
    scale = len(pic.split("V")[1].replace("(", "").replace(")", "")) if "V" in pic else 0
    if "V" in pic and "(" in pic.split("V")[1]:
        scale = int(re.search(r"\((\d+)\)", pic.split("V")[1]).group(1))
    return sign * int(digits), scale


def ts_shape(value):
    return re.sub(r"\d", "n", value)


SOURCE_IS_SYSTEM = lambda rec: rec.get("TRAN-SOURCE", "").strip() == "System"  # noqa: E731
CLOCK = ("COBOL CURRENT-DATE is frozen at 2022-07-18 00:00:00 (COB_CURRENT_DATE); the Java jobs stamp "
         "Clock.systemUTC(), the wall clock of the run. Compared by format (digits masked), not value.")
FILLER_ZEROS = ("FILLER: zeros in the app/data source, blank from RecordWriter (fields without a column are "
                "written as spaces; the table holds no FILLER). Not a data field.")

# (file, field) -> (reason, applies-to-record predicate or None = every record). Masked fields are
# compared by timestamp format; FILLER exclusions not at all.
ACCEPTED = {
    "TRAN-PROC-TS": (CLOCK, None),
    "TRAN-ORIG-TS@System": (CLOCK + " Interest rows only (TRAN-SOURCE 'System': CBACT04C stamps both "
                            "timestamps with the run time); the posted rows carry DALYTRAN's TRAN-ORIG-TS "
                            "and compare exactly.", SOURCE_IS_SYSTEM),
    "FILLER": (FILLER_ZEROS, None),
    "DALYTRAN-PROC-TS": ("transaction.daily_transactions stands in for the DALYTRAN GDG, which CBTRN02C only reads. "
                         "POSTTRAN marks each staged row processed and stamps its processing time (proc_ts) so a "
                         "rerun does not post it twice (idempotent posting, UNT149-12); the source field is blank. "
                         "Excluded.", None),
}


class File:
    def __init__(self, name, cobol, java, lay, masks=(), exclude=(), text=False):
        self.name, self.cobol, self.java, self.lay = name, cobol, java, lay
        self.masks, self.exclude, self.text = masks, exclude, text


def split(data, lrecl):
    return [data[i:i + lrecl] for i in range(0, len(data), lrecl)]


def compare_file(f: File):
    """-> (summary row, [difference lines], unexplained count)."""
    if not f.cobol.exists() or not f.java.exists():
        missing = [str(p) for p in (f.cobol, f.java) if not p.exists()]
        return [f.name, "-", "-", "missing", "-", "**missing**"], [f"`{f.name}` missing: {', '.join(missing)}"], 1
    a, b = f.cobol.read_bytes(), f.java.read_bytes()
    identical = a == b
    diffs, unexplained, masked = [], 0, 0
    if f.text:
        ra, rb = a.decode("latin-1").splitlines(), b.decode("latin-1").splitlines()
        for n in range(max(len(ra), len(rb))):
            la = ra[n] if n < len(ra) else "<none>"
            lb = rb[n] if n < len(rb) else "<none>"
            if la != lb:
                unexplained += 1
                diffs.append(f"`{f.name}` line {n + 1}: COBOL `{la.rstrip()}` Java `{lb.rstrip()}` **unexplained**")
        if not identical and not unexplained:
            unexplained += 1
            diffs.append(f"`{f.name}`: same lines, different bytes (line terminators or trailing bytes; "
                         f"COBOL {len(a)} bytes, Java {len(b)}) **unexplained**")
        count = (len(ra), len(rb))
    else:
        lrecl, fields = f.lay
        ra, rb = split(a, lrecl), split(b, lrecl)
        count = (len(ra), len(rb))
        if len(a) % lrecl or len(b) % lrecl:
            unexplained += 1
            diffs.append(f"`{f.name}`: size not a multiple of {lrecl} (COBOL {len(a)}, Java {len(b)}) **unexplained**")
        if len(ra) != len(rb):
            unexplained += 1
            diffs.append(f"`{f.name}`: {len(ra)} COBOL records, {len(rb)} Java records **unexplained**")
        for n, (x, y) in enumerate(zip(ra, rb), 1):
            if x == y:
                continue
            rec = {name: x[o:o + ln].decode("latin-1") for name, o, ln, _, _ in fields}
            for name, o, ln, numeric, pic in fields:
                va, vb = x[o:o + ln].decode("latin-1"), y[o:o + ln].decode("latin-1")
                if va == vb:
                    continue
                if name in f.exclude:
                    continue
                if numeric and zoned(va, pic) == zoned(vb, pic):
                    continue
                mask = next((m for m in f.masks if m.split("@")[0] == name
                             and (ACCEPTED[m][1] is None or ACCEPTED[m][1](rec))), None)
                if mask:
                    masked += 1
                    if ts_shape(va) == ts_shape(vb):
                        continue
                    diffs.append(f"`{f.name}` record {n} {name}: format COBOL `{va}` Java `{vb}` **unexplained**")
                else:
                    diffs.append(f"`{f.name}` record {n} {name}: COBOL `{va}` Java `{vb}` **unexplained**")
                unexplained += 1
    notes = ", ".join(f.masks + tuple(f"{e} excluded" for e in f.exclude)) or "-"
    verdict = "reconciled" if unexplained == 0 else f"**{unexplained} unexplained**"
    return ([f.name, f"{count[0]} / {count[1]}", "yes" if identical else "no",
             f"{masked} masked" if masked else "0", notes, verdict], diffs, unexplained)


def rc_file(job, c, j):
    return File("RETURN-CODE", c / job / "RETURN-CODE", j / job / "RETURN-CODE", None, text=True)


TRAN = layout("TranRecordLayout")
TRNX = layout("TrnxRecordLayout")
TS_MASKS = ("TRAN-PROC-TS", "TRAN-ORIG-TS@System")


def jobs(c: Path, j: Path):
    acct, tcat = layout("AccountRecordLayout"), layout("TranCatBalRecordLayout")
    after = j / "EXPORT-INTCALC"
    trnx_masks = tuple(m.replace("TRAN-", "TRNX-") for m in TS_MASKS)
    for m in trnx_masks:
        ACCEPTED.setdefault(m, (ACCEPTED[m.replace("TRNX-", "TRAN-")][0],
                                (lambda r: r.get("TRNX-SOURCE", "").strip() == "System") if "@" in m else None))
    unchanged = [("CUSTDATA", "custdata.txt", "CustomerRecordLayout", ()),
                 ("CARDDATA", "carddata.txt", "CardRecordLayout", ()),
                 ("CARDXREF", "cardxref.txt", "CardXrefRecordLayout", ()),
                 ("TRANTYPE", "trantype.txt", "TranTypeRecordLayout", ("FILLER",)),
                 ("TRANCATG", "trancatg.txt", "TranCatRecordLayout", ("FILLER",)),
                 ("DISCGRP", "discgrp.txt", "DisGroupRecordLayout", ("FILLER",)),
                 ("DALYTRAN", "dailytran.txt", "DalytranRecordLayout", ("DALYTRAN-PROC-TS",))]
    return {
        "POSTTRAN": [
            File("TRANSACT", c / "POSTTRAN/TRANSACT", j / "POSTTRAN/TRANSACT", TRAN, TS_MASKS),
            File("DALYREJS", c / "POSTTRAN/DALYREJS", j / "POSTTRAN/DALYREJS", layout("DalyrejsRecordLayout")),
            File("TCATBALF", c / "POSTTRAN/TCATBALF", j / "POSTTRAN/TCATBALF", tcat, exclude=("FILLER",)),
            File("ACCTFILE (account.accounts export)", c / "POSTTRAN/ACCTFILE", j / "EXPORT-POSTTRAN/ACCTDATA", acct,
                 exclude=("FILLER",)),
            File("TCATBALF (transaction.category_balances export)", c / "POSTTRAN/TCATBALF",
                 j / "EXPORT-POSTTRAN/TCATBALF", tcat, exclude=("FILLER",)),
            rc_file("POSTTRAN", c, j),
        ],
        "INTCALC": [
            File("SYSTRAN", c / "INTCALC/SYSTRAN", j / "INTCALC/SYSTRAN", TRAN, TS_MASKS),
            File("ACCTFILE (account.accounts export)", c / "INTCALC/ACCTFILE", after / "ACCTDATA", acct,
                 exclude=("FILLER",)),
            File("TCATBALF (transaction.category_balances export)", c / "POSTTRAN/TCATBALF", after / "TCATBALF", tcat,
                 exclude=("FILLER",)),
            rc_file("INTCALC", c, j),
        ] + [File(f"{ds} (export vs app/data, not written by the chain)", DATA / src, after / ds, layout(lay),
                  exclude=excl, text=False) for ds, src, lay, excl in unchanged],
        "TRANREPT": [
            File("TRANSACT.BKUP (transaction.transactions unload)", c / "COMBTRAN/TRANSACT",
                 j / "TRANREPT/TRANSACT.BKUP", TRAN, TS_MASKS),
            File("TRANSACT.DALY", c / "TRANREPT/TRANSACT.DALY", j / "TRANREPT/TRANSACT.DALY", TRAN, TS_MASKS),
            File("TRANREPT", c / "TRANREPT/TRANREPT", j / "TRANREPT/TRANREPT", None, text=True),
            rc_file("TRANREPT", c, j),
        ],
        "CREASTMT": [
            File("TRXFL.SEQ", c / "CREASTMT/TRXFL.SEQ", j / "CREASTMT/TRXFL.SEQ", TRNX, trnx_masks),
            File("STMTFILE", c / "CREASTMT/STMTFILE", j / "CREASTMT/STMTFILE", None, text=True),
            File("HTMLFILE", c / "CREASTMT/HTMLFILE", j / "CREASTMT/HTMLFILE", None, text=True),
            rc_file("CREASTMT", c, j),
        ],
    }


def main():
    c, j, rep = (Path(p).resolve() for p in sys.argv[1:4])
    notes = (HERE / "notes").resolve()
    total = 0
    for job, files in jobs(c, j).items():
        rows, diffs, bad, used = [], [], 0, set()
        for f in files:
            if f.lay is not None and f.cobol.exists() and str(f.cobol).startswith(str(DATA)):
                # app/data extracts are line sequential: compare the records, not the line ends.
                f.cobol = fixed_copy(f.cobol, f.lay[0], rep / job)
            row, d, u = compare_file(f)
            rows.append(row)
            diffs += d
            bad += u
            used.update(m for m in f.masks + f.exclude if m in ACCEPTED)
        total += bad
        out = rep / job
        out.mkdir(parents=True, exist_ok=True)
        md = [f"# {job} reconciliation", "",
              f"GnuCOBOL golden set vs Java CLI run on a freshly loaded database (`modernization/verification/run.sh`). "
              f"Result: **{'reconciled' if bad == 0 else f'{bad} unexplained differences'}**.", "",
              "| File | Records COBOL / Java | Bytes identical | Masked values | Masks / exclusions | Result |",
              "|---|---|---|---|---|---|"]
        md += ["| " + " | ".join(r) + " |" for r in rows]
        md += ["", "## Accepted deviations", ""]
        md += [f"- `{m}`: {ACCEPTED[m][0]}" for m in sorted(used)] or ["- none"]
        note = notes / f"{job}.md"
        if note.exists():
            md += ["", note.read_text().rstrip()]
        md += ["", "## Differences", ""]
        md += [f"- {d}" for d in diffs[:200]] or ["- none"]
        if len(diffs) > 200:
            md.append(f"- ... {len(diffs) - 200} more")
        (out / "reconciliation.md").write_text("\n".join(md) + "\n")
        print(f"{job:<9} {'reconciled' if bad == 0 else f'{bad} UNEXPLAINED'}  -> {out / 'reconciliation.md'}")
    return 1 if total else 0


def fixed_copy(src, lrecl, out_dir):
    out_dir.mkdir(parents=True, exist_ok=True)
    dst = out_dir / f".{src.name}.fixed"
    dst.write_bytes(b"".join(l.rstrip(b"\r")[:lrecl].ljust(lrecl, b" ")
                             for l in src.read_bytes().split(b"\n") if l.rstrip(b"\r")))
    return dst


if __name__ == "__main__":
    sys.exit(main())
