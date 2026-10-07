#!/usr/bin/env python3
"""GnuCOBOL side of the golden set: the baseline machinery (scripts/baseline/baseline.py) run on the after-online
datasets written by apply_online_scenario.py instead of the pristine samples.

Same compile flags, clock pins and z/OS emulations as the committed baseline (COB_CURRENT_DATE=2022-07-06,
INTCALC PARM 2022071800, DATEPARM 2022-01-01..2022-07-06). Job order:

  1. IDCAMS loads of the ten KSDS (ACCTDATA/CUSTDATA/CARDDATA/CARDXREF/TRANSACT/USRSEC from the after-online files,
     the reference files TRANTYPE/TRANCATG/DISCGRP/TCATBALF from the samples as in the baseline)
  2. ONLINE-TRANREPT: the TRANREPT job on the after-online TRANSACT (the batch report for the window of the online
     Custom report; compared byte for byte with the report the Java API returned)
  3. the nightly cycle in the order of ADR-0016 / 06-scheduling.md: READACCT READCARD READCUST READXREF CBTRN01C
     POSTTRAN INTCALC TRANBKP COMBTRAN TRANREPT CREASTMT PRTCATBL
  4. FINAL/<DATASET>.txt: the KSDS after the cycle, unloaded in key order (one raw record per line)

<out>/<JOB>/ has the layout of docs/validation/baseline/<JOB>/, so scripts/batch/compare_*.py read it through
CARDDEMO_BASELINE_DIR. <out>/jobs.json lists every job with its RC.
"""
from __future__ import annotations

import argparse
import json
import shutil
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parents[0] / "baseline"))
import baseline  # noqa: E402
from layouts import read_records as read_lines, write_records as write_lines  # noqa: E402

AFTER_ONLINE = ("ACCTDATA", "CUSTDATA", "CARDDATA", "CARDXREF", "TRANSACT", "USRSEC")
FINAL = ("ACCTDATA", "CUSTDATA", "CARDDATA", "CARDXREF", "TRANSACT", "TCATBALF", "USRSEC")


class GoldenCycle(baseline.Baseline):
    def __init__(self, after_online: Path):
        super().__init__(fast=True)
        self.after_online = after_online

    def prepare_data(self):
        super().prepare_data()
        for name in AFTER_ONLINE:
            lrecl = baseline.KSDS[name]["lrecl"]
            recs = [r.encode("latin-1") for r in read_lines(self.after_online / f"{name}.txt", lrecl)]
            baseline.write_records(self.data / f"{name}.dat", recs)
            text, _, _ = baseline.fold(self.data / f"{name}.dat", lrecl)
            baseline.write(baseline.OUT / "00-DATA" / f"{name}.txt", text)
            self.data_rows.append((name, f"after-online ({self.after_online.name}/{name}.txt)", lrecl, len(recs), ""))

    def job_load_tranfile(self):
        self._load_job("TRANFILE", "TRANSACT", "TRANSACT", "TRANFILE.jcl",
                       "- Golden set: loaded from the after-online TRANSACT (the online transactions of the scenario).")

    def job_online_tranrept(self):
        real = self.finish_job

        def renamed(job, *args, **kw):
            return real("ONLINE-TRANREPT", *args, **kw)
        self.finish_job = renamed
        try:
            self.job_tranrept()
        finally:
            self.finish_job = real
        rept = self.cat.cur_gen("AWS.M2.CARDDEMO.TRANREPT")
        shutil.copyfile(rept, baseline.OUT / "ONLINE-TRANREPT" / "TRANREPT.raw")

    def unload_final(self):
        d = baseline.OUT / "FINAL"
        d.mkdir(parents=True, exist_ok=True)
        for name in FINAL:
            tmp = baseline.WORK / "ds" / f"_final_{name}.dat"
            tmp.unlink(missing_ok=True)
            if self.idcams_unload(name, tmp, []):
                raise RuntimeError(f"unload of {name} failed")
            recs = baseline.read_records(tmp, baseline.KSDS[name]["lrecl"]) if tmp.exists() else []
            write_lines(d / f"{name}.txt", [r.decode("latin-1") for r in recs])

    def run(self):
        for p in (baseline.OUT, baseline.WORK):
            if p.exists():
                shutil.rmtree(p)
        for p in (baseline.OUT, self.bin, self.gen, self.data, baseline.WORK / "ds", baseline.WORK / "ksds",
                  baseline.WORK / "gdg"):
            p.mkdir(parents=True, exist_ok=True)
        self.log_versions()
        self.compile_all()
        self.prepare_data()
        self.build_idxutils()
        for job in [self.job_load_acctfile, self.job_load_cardfile, self.job_load_custfile,
                    self.job_load_xreffile, self.job_load_tranfile, self.job_load_trantype,
                    self.job_load_trancatg, self.job_load_discgrp, self.job_load_tcatbalf,
                    self.job_load_dusrsecj, self.job_online_tranrept,
                    self.job_readacct, self.job_readcard, self.job_readcust, self.job_readxref,
                    self.job_cbtrn01c, self.job_posttran, self.job_intcalc, self.job_tranbkp,
                    self.job_combtran, self.job_tranrept, self.job_creastmt, self.job_prtcatbl]:
            job()
        self.unload_final()
        (baseline.OUT / "jobs.json").write_text(json.dumps(
            [{"job": j, "programs": p, "rc": rc, "outputs": o} for j, p, rc, o, _ in self.job_rows], indent=1) + "\n")


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--after-online", type=Path, required=True)
    ap.add_argument("--out", type=Path, required=True)
    ap.add_argument("--work", type=Path, required=True)
    a = ap.parse_args()
    baseline.OUT = a.out.resolve()
    baseline.WORK = a.work.resolve()
    if not shutil.which("cobc"):
        raise SystemExit("cobc (GnuCOBOL) not found")
    GoldenCycle(a.after_online.resolve()).run()
    print(f"cobol_cycle: outputs in {a.out}")


if __name__ == "__main__":
    main()
