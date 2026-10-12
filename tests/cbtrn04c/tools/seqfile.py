#!/usr/bin/env python3
"""Fixed-length record file helpers for the CBTRN04C test harness.

GnuCOBOL ORGANIZATION SEQUENTIAL files are raw fixed-length records with
no record separators. The repository sample data under app/data/ASCII is
one record per text line (some files with CRLF endings and trailing
blanks trimmed), so these helpers convert between the two forms.

  txt2seq   <in.txt> <out.dat> <lrecl>   text lines  -> fixed records
  seq2txt   <in.dat> <out.txt> <lrecl>   fixed records -> text lines
  normalize <in.txt> <out.txt> <lrecl>   text lines  -> text lines,
                                         CR stripped, padded to lrecl
"""
import sys


def _lines(data: bytes):
    for line in data.split(b"\n"):
        line = line.rstrip(b"\r")
        if line:
            yield line


def _fit(line: bytes, lrecl: int) -> bytes:
    if len(line) > lrecl:
        raise SystemExit(f"record longer than {lrecl}: {len(line)} bytes")
    return line.ljust(lrecl)


def txt2seq(src: str, dst: str, lrecl: int) -> None:
    with open(src, "rb") as fh:
        recs = [_fit(ln, lrecl) for ln in _lines(fh.read())]
    with open(dst, "wb") as out:
        out.write(b"".join(recs))


def seq2txt(src: str, dst: str, lrecl: int) -> None:
    with open(src, "rb") as fh:
        data = fh.read()
    if len(data) % lrecl:
        raise SystemExit(
            f"{src}: {len(data)} bytes is not a multiple of LRECL {lrecl}")
    with open(dst, "wb") as out:
        for i in range(0, len(data), lrecl):
            out.write(data[i:i + lrecl] + b"\n")


def normalize(src: str, dst: str, lrecl: int) -> None:
    with open(src, "rb") as fh:
        recs = [_fit(ln, lrecl) for ln in _lines(fh.read())]
    with open(dst, "wb") as out:
        out.write(b"".join(r + b"\n" for r in recs))


COMMANDS = {"txt2seq": txt2seq, "seq2txt": seq2txt, "normalize": normalize}


def main(argv):
    if len(argv) != 5 or argv[1] not in COMMANDS:
        sys.stderr.write(__doc__)
        return 2
    COMMANDS[argv[1]](argv[2], argv[3], int(argv[4]))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
