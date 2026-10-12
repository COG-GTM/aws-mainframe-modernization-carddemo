#!/usr/bin/env python3
"""Compile the COBOL program and compare all deterministic test cases."""

from __future__ import annotations

import argparse
import os
import re
import subprocess
import sys
from pathlib import Path
from typing import Any

import gen_fixtures

ROOT = gen_fixtures.ROOT
TEST_DIR = gen_fixtures.TEST_DIR
BUILD_DIR = TEST_DIR / "build"
WORK_DIR = TEST_DIR / "work"
PROGRAM = BUILD_DIR / "CBGLX01C"
AMOUNT_RE = re.compile(rb"-\s*[0-9,]+\.[0-9]{2}|[0-9,]+\.[0-9]{2}")
INTEGER_RE = re.compile(rb"[0-9][0-9,]*")


def compile_program() -> tuple[bool, str]:
    BUILD_DIR.mkdir(parents=True, exist_ok=True)
    command = [
        "cobc",
        "-x",
        "--std=ibm-strict",
        "-fsign=EBCDIC",
        "-I",
        str(ROOT / "app/cpy"),
        str(ROOT / "app/cbl/CBGLX01C.cbl"),
        "-o",
        str(PROGRAM),
    ]
    result = subprocess.run(
        command,
        cwd=ROOT,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        check=False,
    )
    return result.returncode == 0, result.stdout


def _amount_cents(raw: bytes) -> int:
    value = raw.decode("ascii").replace(",", "").replace(" ", "").strip()
    negative = value.startswith("-")
    value = value.lstrip("+-")
    whole, fraction = value.split(".")
    cents = int(whole) * 100 + int(fraction)
    return -cents if negative else cents


def _signed_field(raw: bytes) -> int:
    if raw[:1] not in (b"+", b"-") or not raw[1:].isdigit():
        raise ValueError(f"invalid signed interface field: {raw!r}")
    cents = int(raw[1:])
    return -cents if raw[:1] == b"-" else cents


def interface_totals(data: bytes | None) -> dict[str, int]:
    totals = {
        "details": 0,
        "transactions": 0,
        "debits": 0,
        "credits": 0,
        "net": 0,
        "hash": 0,
        "trailer_details": 0,
        "trailer_transactions": 0,
        "trailer_debits": 0,
        "trailer_credits": 0,
        "trailer_net": 0,
        "trailer_hash": 0,
    }
    if data is None:
        return totals
    if len(data) % 150:
        raise ValueError(f"interface has non-150-byte length {len(data)}")
    for offset in range(0, len(data), 150):
        record = data[offset : offset + 150]
        if record[:1] == b"D":
            totals["details"] += 1
            totals["transactions"] += int(record[27:36])
            debit = _signed_field(record[36:52])
            credit = _signed_field(record[52:68])
            totals["debits"] += debit
            totals["credits"] += credit
            totals["net"] += _signed_field(record[68:84])
            totals["hash"] += int(record[10:21])
        elif record[:1] == b"T":
            totals["trailer_details"] = int(record[1:10])
            totals["trailer_transactions"] = int(record[10:21])
            totals["trailer_debits"] = _signed_field(record[21:39])
            totals["trailer_credits"] = _signed_field(record[39:57])
            totals["trailer_net"] = _signed_field(record[57:75])
            totals["trailer_hash"] = int(record[75:93])
    return totals


def _integer_values(line: bytes) -> list[int]:
    return [int(match.replace(b",", b"")) for match in INTEGER_RE.findall(line)]


def _amount_values(line: bytes) -> list[int]:
    return [_amount_cents(match) for match in AMOUNT_RE.findall(line)]


def check_report(
    report: bytes,
    expected_rc: int,
    oracle: dict[str, Any],
    interface: bytes | None,
) -> list[str]:
    errors: list[str] = []
    if len(report) % 132:
        errors.append(
            f"report length {len(report)} is not a multiple of 132 bytes"
        )
    lines = [
        report[offset : offset + 132].rstrip(b" ")
        for offset in range(0, len(report), 132)
    ]
    expected_title = (
        b"CBGLX01C  LEDGER-SUMMARY INTERFACE GLSUMM01 - "
        b"RECONCILIATION REPORT"
    )
    if not lines or lines[0] != expected_title:
        errors.append(f"report title is incorrect: {lines[0] if lines else b''!r}")
    if len(lines) < 2 or not lines[1].startswith(b"RUN DATE "):
        errors.append("report run-date/source line is missing")

    result_lines = [
        line for line in lines if line.startswith(b"RESULT:")
    ]
    if len(result_lines) != 1:
        errors.append(f"expected one RESULT line, found {len(result_lines)}")
    else:
        result_line = result_lines[0]
        rc_match = re.search(rb"RETURN CODE\s+([0-9]{4})", result_line)
        if rc_match is None or int(rc_match.group(1)) != expected_rc:
            errors.append(f"RESULT line has wrong return code: {result_line!r}")
        expected_result = {
            0: b"BALANCED",
            4: b"BALANCED WITH WARNINGS",
            8: b"OUT OF BALANCE",
            12: b"RUN ABORTED",
        }.get(expected_rc)
        if expected_result is None or expected_result not in result_line:
            errors.append(f"RESULT line has wrong outcome: {result_line!r}")
        if expected_rc == 0 and b"WITH WARNINGS" in result_line:
            errors.append(f"clean result is labeled with warnings: {result_line!r}")

    input_labels = (
        b"TRANSACT RECORDS READ",
        b"ACCEPTED",
        b"REJECTED",
        b"ACCEPTED WITH WARNINGS",
        b"XREF ENTRIES LOADED",
        b"TCATBALF RECORDS READ",
    )
    for label in input_labels:
        rows = [
            line for line in lines if line[:40].rstrip() == label
        ]
        if len(rows) != 1 or len(_integer_values(rows[0])) != 1:
            errors.append(f"expected one aligned input count for {label.decode()}")
        elif not rows[0][63:64].isdigit():
            errors.append(f"input count {label.decode()} is not right-aligned")

    exception_sections = [
        index for index, line in enumerate(lines)
        if line == b"EXCEPTIONS"
    ]
    control_sections = [
        index for index, line in enumerate(lines)
        if line == b"CONTROL TOTALS"
    ]
    if len(exception_sections) != 1:
        errors.append(
            f"expected one EXCEPTIONS section, found {len(exception_sections)}"
        )
    elif len(control_sections) == 1:
        exception_start = exception_sections[0]
        exception_end = control_sections[0]
        headers = [
            line for line in lines[exception_start + 1 : exception_end]
            if line.startswith(b"CODE")
        ]
        expected_exception_header = (
            b"CODE", b"KEY", b"DESCRIPTION"
        )
        if len(headers) != 1 or (
            headers[0][0:4] != expected_exception_header[0]
            or headers[0][6:9] != expected_exception_header[1]
            or headers[0][38:49] != expected_exception_header[2]
        ):
            errors.append("exception table header is not aligned")
        exception_lines = [
            line for line in lines[exception_start + 1 : exception_end]
            if line[:3] in {
                b"E01", b"E02", b"E03", b"E04", b"E05", b"E06",
                b"R01", b"R02", b"R03", b"R04", b"W01", b"W02",
                b"W03", b"B01", b"B02", b"B03", b"B04", b"B05",
                b"B06",
            }
        ]
        expected_exceptions = oracle.get("exceptions", [])
        expected_codes = [
            exception.split(maxsplit=1)[0].encode("ascii")
            for exception in expected_exceptions
        ]
        actual_codes = [line[:3] for line in exception_lines]
        if actual_codes != expected_codes[:50]:
            errors.append(
                f"exception codes {actual_codes} do not match oracle "
                f"{expected_codes[:50]}"
            )
        cap_lines = [
            line for line in lines[exception_start + 1 : exception_end]
            if b"MORE EXCEPTIONS NOT LISTED" in line
        ]
        expected_more = max(0, len(expected_exceptions) - 50)
        if expected_more:
            if len(cap_lines) != 1:
                errors.append(
                    f"expected one exception-cap line for {expected_more} "
                    f"omitted rows, found {len(cap_lines)}"
                )
            else:
                cap_match = re.search(
                    rb"([0-9,]+)\s+MORE EXCEPTIONS NOT LISTED",
                    cap_lines[0],
                )
                cap_count = (
                    int(cap_match.group(1).replace(b",", b""))
                    if cap_match is not None
                    else None
                )
                if cap_count != expected_more:
                    errors.append(
                        f"exception-cap line reports {cap_count}, "
                        f"expected {expected_more}"
                    )
        elif cap_lines:
            errors.append("exception-cap line is present without omitted rows")

    control_headers = []
    if len(control_sections) == 1:
        control_start = control_sections[0]
        next_sections = [
            index for index in range(control_start + 1, len(lines))
            if lines[index] == b"LEDGER TIE-OUT"
        ]
        control_end = next_sections[0] if next_sections else len(lines)
        control_headers = [
            line for line in lines[control_start + 1 : control_end]
            if line.startswith(b"ITEM")
        ]
    if len(control_headers) != 1 or (
        control_headers[0][0:4] != b"ITEM"
        or control_headers[0][26:32] != b"SOURCE"
        or control_headers[0][52:63] != b"FILE DETAIL"
        or control_headers[0][78:90] != b"FILE TRAILER"
        or control_headers[0][108:114] != b"STATUS"
    ):
        errors.append("control-total header must fit one aligned row")

    totals = interface_totals(interface)
    source = {
        "transactions": int(oracle["accepted"]),
        "debits": int(oracle["debits"]),
        "credits": int(oracle["credits"]),
        "net": int(oracle["net"]),
        "details": int(oracle["details"]),
        "hash": int(oracle["hash"]),
    }
    expected_rows = {
        b"TRANSACTIONS": (
            [source["transactions"], totals["transactions"],
             totals["trailer_transactions"]],
            _integer_values,
        ),
        b"DEBITS": (
            [source["debits"], totals["debits"], totals["trailer_debits"]],
            _amount_values,
        ),
        b"CREDITS": (
            [source["credits"], totals["credits"], totals["trailer_credits"]],
            _amount_values,
        ),
        b"NET": (
            [source["net"], totals["net"], totals["trailer_net"]],
            _amount_values,
        ),
        b"DETAIL RECORDS": (
            [source["details"], totals["details"], totals["trailer_details"]],
            _integer_values,
        ),
        b"HASH TOTAL": (
            [source["hash"], totals["hash"], totals["trailer_hash"]],
            _integer_values,
        ),
    }
    for label, (expected_values, parser) in expected_rows.items():
        rows = [
            line for line in lines
            if line.startswith(label)
            and len(parser(line)) == len(expected_values)
        ]
        if len(rows) != 1:
            errors.append(f"expected one {label.decode()} control row, found {len(rows)}")
            continue
        actual_values = parser(rows[0])
        if actual_values != expected_values:
            errors.append(
                f"{label.decode()} row values {actual_values} "
                f"do not match oracle/interface {expected_values}"
            )
        if len(expected_values) == 3:
            if any(
                not rows[0][end - 1 : end].isdigit()
                for end in (52, 78, 104)
            ):
                errors.append(
                    f"{label.decode()} values do not share aligned columns"
                )
            should_match = len(set(expected_values)) == 1
            status = rows[0].split()[-1]
            expected_status = b"OK" if should_match else b"DIFF"
            if status != expected_status:
                errors.append(
                    f"{label.decode()} status {status!r}, expected "
                    f"{expected_status.decode()}"
                )

    tie_out_labels = (
        b"KEYS TIED",
        b"KEYS DIFFERING",
        b"KEYS MISSING FROM TCATBALF",
        b"BALANCE-ONLY KEYS (NON-ZERO)",
    )
    for label in tie_out_labels:
        rows = [line for line in lines if line.startswith(label)]
        if len(rows) != 1 or len(_integer_values(rows[0])) != 1:
            errors.append(f"expected one tie-out count for {label.decode()}")
        elif not rows[0][63:64].isdigit():
            errors.append(f"tie-out count {label.decode()} is not right-aligned")

    feeder_headers = [
        line for line in lines if line[26:34] == b"EXPECTED"
    ]
    if feeder_headers:
        if len(feeder_headers) != 1 or (
            feeder_headers[0][26:34] != b"EXPECTED"
            or feeder_headers[0][52:58] != b"ACTUAL"
            or feeder_headers[0][108:114] != b"STATUS"
        ):
            errors.append("feeder control-total header is not aligned")
    return errors


def run_case(
    case: gen_fixtures.Case,
    *,
    update_reports: bool,
    verbose: bool,
    oracle_only: bool,
) -> tuple[int, int, bool, bool, list[str], bytes]:
    work = WORK_DIR / case.name
    paths = gen_fixtures.write_inputs(case, work)
    actual_output = work / "GLSUMOUT.dat"
    actual_report = work / "GLRECRPT.rpt"
    actual_output.unlink(missing_ok=True)
    actual_report.unlink(missing_ok=True)
    env = {
        "DD_GLPARM": str(paths["GLPARM"]),
        "DD_TRANFILE": str(paths["TRANFILE"]),
        "DD_XREFFILE": str(paths["XREFFILE"]),
        "DD_TCATBALF": str(paths["TCATBALF"]),
        "DD_GLSUMOUT": str(actual_output),
        "DD_GLRECRPT": str(actual_report),
        "DD_SORTWK01": str(work / "SORTWK01.dat"),
    }
    if "TRANFILE" in case.missing:
        paths["TRANFILE"].unlink(missing_ok=True)
    process_env = os.environ.copy()
    process_env.update(env)
    result = subprocess.run(
        [str(PROGRAM)],
        cwd=ROOT,
        env=process_env,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        check=False,
        timeout=180,
    )
    actual_rc = result.returncode
    expected = gen_fixtures.oracle(case)
    expected_rc_path = TEST_DIR / "expected" / case.name / "RC"
    expected_rc = (
        int(expected["rc"])
        if oracle_only
        else int(expected_rc_path.read_text().strip())
    )
    interface_ok = expected_rc == expected["rc"] == actual_rc
    expected_interface_file = (
        TEST_DIR / "expected" / case.name / "GLSUMOUT.dat"
    )
    expected_absent = (
        TEST_DIR / "expected" / case.name / "GLSUMOUT.absent"
    ).exists()
    if oracle_only:
        interface_ok = interface_ok and (
            (expected["interface"] is None and not actual_output.exists())
            or (
                expected["interface"] is not None
                and actual_output.exists()
                and actual_output.read_bytes() == expected["interface"]
            )
        )
    elif expected_absent:
        interface_ok = interface_ok and expected["interface"] is None
        interface_ok = interface_ok and not actual_output.exists()
    elif expected_interface_file.exists():
        expected_interface = expected_interface_file.read_bytes()
        interface_ok = (
            interface_ok
            and expected["interface"] == expected_interface
            and actual_output.exists()
            and actual_output.read_bytes() == expected_interface
        )
    else:
        interface_ok = False
    actual_interface = actual_output.read_bytes() if actual_output.exists() else None
    report_errors: list[str] = []
    if actual_report.exists():
        report = actual_report.read_bytes()
        report_errors.extend(
            check_report(report, expected["rc"], expected, expected["interface"])
        )
    else:
        report = b""
        report_errors.append("GLRECRPT was not created")

    report_golden = TEST_DIR / "expected" / case.name / "GLRECRPT.rpt"
    if update_reports and not report_errors:
        report_golden.write_bytes(report)
    if not oracle_only:
        if not report_golden.exists():
            report_errors.append("report golden is missing")
        elif report_golden.read_bytes() != report:
            report_errors.append("report bytes differ from reviewed golden")

    messages = []
    if actual_rc != expected["rc"]:
        messages.append(
            f"actual RC {actual_rc}, oracle/expected RC {expected['rc']}"
        )
    if expected_rc != expected["rc"]:
        messages.append(
            f"stored expected RC {expected_rc}, oracle RC {expected['rc']}"
        )
    if expected["interface"] is None and actual_output.exists():
        messages.append("interface should be absent")
    elif expected["interface"] is not None and not actual_output.exists():
        messages.append("interface file is missing")
    elif (
        expected["interface"] is not None
        and actual_output.exists()
        and actual_output.read_bytes() != expected["interface"]
    ):
        messages.append("interface bytes differ from oracle")
    messages.extend(report_errors)
    if result.stdout:
        output_text = result.stdout.decode(errors="replace").strip()
        if output_text and f"RETURN CODE {expected['rc']:04d}" not in output_text:
            messages.append(f"SYSOUT result differs: {output_text}")
    if verbose:
        print(f"\n--- {case.name} report ---")
        for offset in range(0, len(report), 132):
            print(
                report[offset : offset + 132]
                .decode("ascii", errors="replace")
                .rstrip()
            )
    return (
        expected_rc,
        actual_rc,
        interface_ok,
        not report_errors,
        messages,
        report,
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--update-report-goldens",
        action="store_true",
        help="replace report snapshots after independent semantic checks",
    )
    parser.add_argument(
        "--oracle-only",
        action="store_true",
        help="compare program output directly to the Python model, without stored expectations",
    )
    parser.add_argument("-v", "--verbose", metavar="CASE")
    args = parser.parse_args()
    if args.oracle_only and args.update_report_goldens:
        parser.error("--oracle-only cannot update report goldens")

    compiled, compile_output = compile_program()
    if compile_output:
        print(compile_output, end="")
    if not compiled:
        print("Compilation failed.")
        return 1
    if compile_output.strip():
        print("Compiler emitted output above; review warnings before proceeding.")

    cases = gen_fixtures.all_cases()
    if args.verbose and args.verbose not in {case.name for case in cases}:
        parser.error(f"unknown case: {args.verbose}")
    if not args.oracle_only and not (TEST_DIR / "expected").exists():
        print("Expected files are missing; run gen_fixtures.py --write-expected.")
        return 1

    failures = 0
    print("case                               expRC actRC  interface  report  result")
    print("-" * 77)
    for case in cases:
        expected_rc, actual_rc, interface_ok, report_ok, messages, _ = run_case(
            case,
            update_reports=args.update_report_goldens,
            verbose=args.verbose == case.name,
            oracle_only=args.oracle_only,
        )
        passed = interface_ok and report_ok and not messages
        if not passed:
            failures += 1
        print(
            f"{case.name:34} {expected_rc:5} {actual_rc:5}  "
            f"{'PASS' if interface_ok else 'FAIL':9} "
            f"{'PASS' if report_ok else 'FAIL':6} "
            f"{'PASS' if passed else 'FAIL'}"
        )
        for message in messages:
            print(f"  {message}")
    passed = len(cases) - failures
    print(f"{passed} passed, {failures} failed")
    return 0 if failures == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
