"""Encode/decode fixed-width records using the layouts in app/cpy."""
from decimal import Decimal
from pathlib import Path

import copybook
from harness import APP_CPY, unzoned, zoned


class Layout:
    def __init__(self, cpy, record=None):
        path = APP_CPY / cpy
        if not path.exists():
            path = next(APP_CPY.glob(Path(cpy).stem + ".*"))
        roots = copybook.parse_file(path)
        self.root = roots[0] if record is None else next(r for r in roots if r.name == record)
        self.length = self.root.total_size
        self.fields = {f.name: f for f in copybook.leaf_fields(self.root) if f.name != "FILLER"}

    @staticmethod
    def _numeric(f):
        p = copybook.expand_pic(f.pic)
        return set(p) <= set("S9V")

    @staticmethod
    def _scale(f):
        p = copybook.expand_pic(f.pic)
        return len(p.split("V")[1]) if "V" in p else 0

    def encode(self, **values):
        buf = [" "] * self.length
        for name, f in self.fields.items():
            if self._numeric(f):
                v = values.pop(name, 0)
                text = zoned(v, copybook.pic_digits(f.pic), self._scale(f),
                             signed=f.pic.upper().startswith("S"))
            else:
                text = str(values.pop(name, "")).ljust(f.size)[:f.size]
            buf[f.offset:f.offset + f.size] = text
        if values:
            raise KeyError(f"unknown fields for {self.root.name}: {sorted(values)}")
        return "".join(buf)

    def decode(self, rec):
        out = {}
        for name, f in self.fields.items():
            raw = rec[f.offset:f.offset + f.size]
            if self._numeric(f):
                out[name] = unzoned(raw, self._scale(f))
            else:
                out[name] = raw.rstrip()
        return out


ACCOUNT = Layout("CVACT01Y.cpy")
CARD = Layout("CVACT02Y.cpy")
XREF = Layout("CVACT03Y.cpy")
CUSTOMER = Layout("CVCUS01Y.cpy")
TCATBAL = Layout("CVTRA01Y.cpy")
DISCGRP = Layout("CVTRA02Y.cpy")
TRANTYPE = Layout("CVTRA03Y.cpy")
TRANCATG = Layout("CVTRA04Y.cpy")
TRAN = Layout("CVTRA05Y.cpy")
DALYTRAN = Layout("CVTRA06Y.cpy")
D = Decimal
