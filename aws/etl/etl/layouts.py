"""One layout definition per sample data file.

A layout names the copybook that describes the record, the fixed record length, and one or more
outputs (CSV + optional target table). Each output lists its columns as
``(column, source, transform)`` where ``source`` is a copybook field name, or ``@key`` for a value
supplied by the reader context (record sequence number, parent segment key, ...).
Multi-record files (REDEFINES) select the output by a discriminator field.
"""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass, field
from pathlib import Path

from etl import transforms as t

REPO_ROOT = Path(__file__).resolve().parents[3]
CPY = REPO_ROOT / "app" / "cpy"
PAUTH_CPY = REPO_ROOT / "app" / "app-authorization-ims-db2-mq" / "cpy"
EBCDIC = REPO_ROOT / "app" / "data" / "EBCDIC"
ASCII = REPO_ROOT / "app" / "data" / "ASCII"
IMS_DATA = REPO_ROOT / "app" / "app-authorization-ims-db2-mq" / "data" / "EBCDIC"

SEED_RUN_ID = "SEED"


@dataclass(frozen=True)
class Column:
    name: str
    source: str
    transform: Callable = t.num


@dataclass(frozen=True)
class Output:
    name: str  # CSV file stem
    columns: tuple[Column, ...]
    table: str | None = None  # None -> CSV only (S3 file, not a table)
    copybook: Path | None = None  # overrides Layout.copybook
    discriminator: str | None = None  # value of Layout.discriminator field (or IMS segment name)
    redefine_group: str | None = None  # REDEFINES branch that must contain every source field
    skip_blank: str | None = None  # skip records whose field is blank (VSAM priming records)
    subdir: str = ""


@dataclass(frozen=True)
class Layout:
    name: str
    description: str
    input: Path
    copybook: Path | None
    record_length: int | None
    outputs: tuple[Output, ...]
    discriminator: str | None = None
    reader: str = "fixed"  # "fixed" | "ims"
    ascii: Path | None = None  # equivalent line-sequential ASCII sample, if one exists
    duplicates: tuple[Path, ...] = field(default_factory=tuple)


def C(name: str, source: str, transform: Callable = t.num) -> Column:
    return Column(name, source, transform)


_TRAN_COLUMNS = lambda p: (  # noqa: E731 - CVTRA05Y / CVTRA06Y share the layout with a prefix
    C("tran_id", f"{p}-ID", t.text_nn),
    C("type_cd", f"{p}-TYPE-CD", t.text),
    C("cat_cd", f"{p}-CAT-CD"),
    C("source", f"{p}-SOURCE", t.text),
    C("description", f"{p}-DESC", t.text),
    C("amt", f"{p}-AMT"),
    C("merchant_id", f"{p}-MERCHANT-ID"),
    C("merchant_name", f"{p}-MERCHANT-NAME", t.text),
    C("merchant_city", f"{p}-MERCHANT-CITY", t.text),
    C("merchant_zip", f"{p}-MERCHANT-ZIP", t.text),
    C("card_num", f"{p}-CARD-NUM", t.text_nn),
    C("orig_ts", f"{p}-ORIG-TS", t.timestamp),
    C("proc_ts", f"{p}-PROC-TS", t.timestamp),
)

_EXPORT_HEADER = (
    C("rec_type", "EXPORT-REC-TYPE", t.text_nn),
    C("export_ts", "EXPORT-TIMESTAMP", t.timestamp),
    C("sequence_num", "EXPORT-SEQUENCE-NUM"),
    C("branch_id", "EXPORT-BRANCH-ID", t.text),
    C("region_code", "EXPORT-REGION-CODE", t.text),
)

LAYOUTS: dict[str, Layout] = {}


def _register(layout: Layout) -> None:
    LAYOUTS[layout.name] = layout


_register(
    Layout(
        "usrsec",
        "CSUSR01Y user security (80)",
        EBCDIC / "AWS.M2.CARDDEMO.USRSEC.PS",
        CPY / "CSUSR01Y.cpy",
        80,
        (
            Output(
                "user_security",
                (
                    C("user_id", "SEC-USR-ID", t.upper_text),
                    C("first_name", "SEC-USR-FNAME", t.text_nn),
                    C("last_name", "SEC-USR-LNAME", t.text_nn),
                    C("password_hash", "SEC-USR-PWD", t.bcrypt_upper),
                    C("user_type", "SEC-USR-TYPE", t.text_nn),
                ),
                table="user_security",
            ),
        ),
    )
)

_register(
    Layout(
        "account",
        "CVACT01Y account master (300)",
        EBCDIC / "AWS.M2.CARDDEMO.ACCTDATA.PS",
        CPY / "CVACT01Y.cpy",
        300,
        (
            Output(
                "account",
                (
                    C("acct_id", "ACCT-ID"),
                    C("active_status", "ACCT-ACTIVE-STATUS", t.text_nn),
                    C("curr_bal", "ACCT-CURR-BAL"),
                    C("credit_limit", "ACCT-CREDIT-LIMIT"),
                    C("cash_credit_limit", "ACCT-CASH-CREDIT-LIMIT"),
                    C("open_date", "ACCT-OPEN-DATE", t.date),
                    C("expiration_date", "ACCT-EXPIRAION-DATE", t.date),
                    C("reissue_date", "ACCT-REISSUE-DATE", t.date),
                    C("curr_cyc_credit", "ACCT-CURR-CYC-CREDIT"),
                    C("curr_cyc_debit", "ACCT-CURR-CYC-DEBIT"),
                    C("addr_zip", "ACCT-ADDR-ZIP", t.text),
                    C("group_id", "ACCT-GROUP-ID", t.text),
                ),
                table="account",
            ),
        ),
        ascii=ASCII / "acctdata.txt",
        duplicates=(EBCDIC / "AWS.M2.CARDDEMO.ACCDATA.PS",),
    )
)

_register(
    Layout(
        "card",
        "CVACT02Y card master (150)",
        EBCDIC / "AWS.M2.CARDDEMO.CARDDATA.PS",
        CPY / "CVACT02Y.cpy",
        150,
        (
            Output(
                "card",
                (
                    C("card_num", "CARD-NUM", t.text_nn),
                    C("acct_id", "CARD-ACCT-ID"),
                    C("cvv_cd", "CARD-CVV-CD"),
                    C("embossed_name", "CARD-EMBOSSED-NAME", t.text_nn),
                    C("expiration_date", "CARD-EXPIRAION-DATE", t.date),
                    C("active_status", "CARD-ACTIVE-STATUS", t.text_nn),
                ),
                table="card",
            ),
        ),
        ascii=ASCII / "carddata.txt",
    )
)

_register(
    Layout(
        "cardxref",
        "CVACT03Y card cross-reference (50)",
        EBCDIC / "AWS.M2.CARDDEMO.CARDXREF.PS",
        CPY / "CVACT03Y.cpy",
        50,
        (
            Output(
                "card_xref",
                (
                    C("card_num", "XREF-CARD-NUM", t.text_nn),
                    C("cust_id", "XREF-CUST-ID"),
                    C("acct_id", "XREF-ACCT-ID"),
                ),
                table="card_xref",
            ),
        ),
        ascii=ASCII / "cardxref.txt",
    )
)

_register(
    Layout(
        "customer",
        "CVCUS01Y customer master (500)",
        EBCDIC / "AWS.M2.CARDDEMO.CUSTDATA.PS",
        CPY / "CVCUS01Y.cpy",
        500,
        (
            Output(
                "customer",
                (
                    C("cust_id", "CUST-ID"),
                    C("first_name", "CUST-FIRST-NAME", t.text_nn),
                    C("middle_name", "CUST-MIDDLE-NAME", t.text),
                    C("last_name", "CUST-LAST-NAME", t.text_nn),
                    C("addr_line_1", "CUST-ADDR-LINE-1", t.text),
                    C("addr_line_2", "CUST-ADDR-LINE-2", t.text),
                    C("addr_line_3", "CUST-ADDR-LINE-3", t.text),
                    C("addr_state_cd", "CUST-ADDR-STATE-CD", t.text),
                    C("addr_country_cd", "CUST-ADDR-COUNTRY-CD", t.text),
                    C("addr_zip", "CUST-ADDR-ZIP", t.text),
                    C("phone_num_1", "CUST-PHONE-NUM-1", t.text),
                    C("phone_num_2", "CUST-PHONE-NUM-2", t.text),
                    C("ssn", "CUST-SSN", t.zero_pad9),
                    C("govt_issued_id", "CUST-GOVT-ISSUED-ID", t.text),
                    C("dob", "CUST-DOB-YYYY-MM-DD", t.date),
                    C("eft_account_id", "CUST-EFT-ACCOUNT-ID", t.text),
                    C("pri_card_holder_ind", "CUST-PRI-CARD-HOLDER-IND", t.text),
                    C("fico_credit_score", "CUST-FICO-CREDIT-SCORE"),
                ),
                table="customer",
            ),
        ),
        ascii=ASCII / "custdata.txt",
    )
)

_register(
    Layout(
        "dalytran",
        "CVTRA06Y daily transactions (350) -> staging table",
        EBCDIC / "AWS.M2.CARDDEMO.DALYTRAN.PS",
        CPY / "CVTRA06Y.cpy",
        350,
        (
            Output(
                "daily_transaction",
                (C("run_id", "@run_id", t.text_nn), C("load_seq", "@seq")) + _TRAN_COLUMNS("DALYTRAN"),
                table="daily_transaction",
            ),
        ),
        ascii=ASCII / "dailytran.txt",
    )
)

_register(
    Layout(
        "tranfile_init",
        "CVTRA05Y TRANSACT priming record (TRANFILE.jcl REPRO); all low-values dummy record -> no row",
        EBCDIC / "AWS.M2.CARDDEMO.DALYTRAN.PS.INIT",
        CPY / "CVTRA05Y.cpy",
        350,
        (Output("transaction", _TRAN_COLUMNS("TRAN"), table="transaction", skip_blank="TRAN-ID"),),
    )
)

_register(
    Layout(
        "discgrp",
        "CVTRA02Y disclosure groups (50)",
        EBCDIC / "AWS.M2.CARDDEMO.DISCGRP.PS",
        CPY / "CVTRA02Y.cpy",
        50,
        (
            Output(
                "disclosure_group",
                (
                    C("acct_group_id", "DIS-ACCT-GROUP-ID", t.text_nn),
                    C("type_cd", "DIS-TRAN-TYPE-CD", t.text_nn),
                    C("cat_cd", "DIS-TRAN-CAT-CD"),
                    C("int_rate", "DIS-INT-RATE"),
                ),
                table="disclosure_group",
            ),
        ),
        ascii=ASCII / "discgrp.txt",
    )
)

_register(
    Layout(
        "tcatbal",
        "CVTRA01Y transaction category balances (50)",
        EBCDIC / "AWS.M2.CARDDEMO.TCATBALF.PS",
        CPY / "CVTRA01Y.cpy",
        50,
        (
            Output(
                "tran_cat_balance",
                (
                    C("acct_id", "TRANCAT-ACCT-ID"),
                    C("type_cd", "TRANCAT-TYPE-CD", t.text_nn),
                    C("cat_cd", "TRANCAT-CD"),
                    C("balance", "TRAN-CAT-BAL"),
                ),
                table="tran_cat_balance",
            ),
        ),
        ascii=ASCII / "tcatbal.txt",
    )
)

_register(
    Layout(
        "trancatg",
        "CVTRA04Y transaction categories (60)",
        EBCDIC / "AWS.M2.CARDDEMO.TRANCATG.PS",
        CPY / "CVTRA04Y.cpy",
        60,
        (
            Output(
                "transaction_category",
                (
                    C("type_cd", "TRAN-TYPE-CD", t.text_nn),
                    C("cat_cd", "TRAN-CAT-CD"),
                    C("description", "TRAN-CAT-TYPE-DESC", t.text_nn),
                ),
                table="transaction_category",
            ),
        ),
        ascii=ASCII / "trancatg.txt",
    )
)

_register(
    Layout(
        "trantype",
        "CVTRA03Y transaction types (60)",
        EBCDIC / "AWS.M2.CARDDEMO.TRANTYPE.PS",
        CPY / "CVTRA03Y.cpy",
        60,
        (
            Output(
                "transaction_type",
                (C("type_cd", "TRAN-TYPE", t.text_nn), C("description", "TRAN-TYPE-DESC", t.text_nn)),
                table="transaction_type",
            ),
        ),
        ascii=ASCII / "trantype.txt",
    )
)

_register(
    Layout(
        "export",
        "CVEXPORT multi-record export file (500); REDEFINES chosen by EXPORT-REC-TYPE",
        EBCDIC / "AWS.M2.CARDDEMO.EXPORT.DATA.PS",
        CPY / "CVEXPORT.cpy",
        500,
        (
            Output(
                "export_customer",
                _EXPORT_HEADER
                + (
                    C("cust_id", "EXP-CUST-ID"),
                    C("first_name", "EXP-CUST-FIRST-NAME", t.text_nn),
                    C("middle_name", "EXP-CUST-MIDDLE-NAME", t.text),
                    C("last_name", "EXP-CUST-LAST-NAME", t.text_nn),
                    C("addr_line_1", "EXP-CUST-ADDR-LINE(1)", t.text),
                    C("addr_line_2", "EXP-CUST-ADDR-LINE(2)", t.text),
                    C("addr_line_3", "EXP-CUST-ADDR-LINE(3)", t.text),
                    C("addr_state_cd", "EXP-CUST-ADDR-STATE-CD", t.text),
                    C("addr_country_cd", "EXP-CUST-ADDR-COUNTRY-CD", t.text),
                    C("addr_zip", "EXP-CUST-ADDR-ZIP", t.text),
                    C("phone_num_1", "EXP-CUST-PHONE-NUM(1)", t.text),
                    C("phone_num_2", "EXP-CUST-PHONE-NUM(2)", t.text),
                    C("ssn", "EXP-CUST-SSN", t.zero_pad9),
                    C("govt_issued_id", "EXP-CUST-GOVT-ISSUED-ID", t.text),
                    C("dob", "EXP-CUST-DOB-YYYY-MM-DD", t.date),
                    C("eft_account_id", "EXP-CUST-EFT-ACCOUNT-ID", t.text),
                    C("pri_card_holder_ind", "EXP-CUST-PRI-CARD-HOLDER-IND", t.text),
                    C("fico_credit_score", "EXP-CUST-FICO-CREDIT-SCORE"),
                ),
                discriminator="C",
                redefine_group="EXPORT-CUSTOMER-DATA",
                subdir="export",
            ),
            Output(
                "export_account",
                _EXPORT_HEADER
                + (
                    C("acct_id", "EXP-ACCT-ID"),
                    C("active_status", "EXP-ACCT-ACTIVE-STATUS", t.text_nn),
                    C("curr_bal", "EXP-ACCT-CURR-BAL"),
                    C("credit_limit", "EXP-ACCT-CREDIT-LIMIT"),
                    C("cash_credit_limit", "EXP-ACCT-CASH-CREDIT-LIMIT"),
                    C("open_date", "EXP-ACCT-OPEN-DATE", t.date),
                    C("expiration_date", "EXP-ACCT-EXPIRAION-DATE", t.date),
                    C("reissue_date", "EXP-ACCT-REISSUE-DATE", t.date),
                    C("curr_cyc_credit", "EXP-ACCT-CURR-CYC-CREDIT"),
                    C("curr_cyc_debit", "EXP-ACCT-CURR-CYC-DEBIT"),
                    C("addr_zip", "EXP-ACCT-ADDR-ZIP", t.text),
                    C("group_id", "EXP-ACCT-GROUP-ID", t.text),
                ),
                discriminator="A",
                redefine_group="EXPORT-ACCOUNT-DATA",
                subdir="export",
            ),
            Output(
                "export_transaction",
                _EXPORT_HEADER + _TRAN_COLUMNS("EXP-TRAN"),
                discriminator="T",
                redefine_group="EXPORT-TRANSACTION-DATA",
                subdir="export",
            ),
            Output(
                "export_card_xref",
                _EXPORT_HEADER
                + (
                    C("card_num", "EXP-XREF-CARD-NUM", t.text_nn),
                    C("cust_id", "EXP-XREF-CUST-ID"),
                    C("acct_id", "EXP-XREF-ACCT-ID"),
                ),
                discriminator="X",
                redefine_group="EXPORT-CARD-XREF-DATA",
                subdir="export",
            ),
            Output(
                "export_card",
                _EXPORT_HEADER
                + (
                    C("card_num", "EXP-CARD-NUM", t.text_nn),
                    C("acct_id", "EXP-CARD-ACCT-ID"),
                    C("cvv_cd", "EXP-CARD-CVV-CD"),
                    C("embossed_name", "EXP-CARD-EMBOSSED-NAME", t.text_nn),
                    C("expiration_date", "EXP-CARD-EXPIRAION-DATE", t.date),
                    C("active_status", "EXP-CARD-ACTIVE-STATUS", t.text_nn),
                ),
                discriminator="D",
                redefine_group="EXPORT-CARD-DATA",
                subdir="export",
            ),
        ),
        discriminator="EXPORT-REC-TYPE",
    )
)

_register(
    Layout(
        "dbpautp0",
        "IMS HIDAM DBPAUTP0 HD unload: PAUTSUM0 (CIPAUSMY) + PAUTDTL1 (CIPAUDTY)",
        IMS_DATA / "AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat",
        None,
        None,
        (
            Output(
                "pending_auth_summary",
                (
                    C("acct_id", "PA-ACCT-ID"),
                    C("cust_id", "PA-CUST-ID"),
                    C("auth_status", "PA-AUTH-STATUS", t.text),
                    C("account_status", "@account_status"),
                    C("credit_limit", "PA-CREDIT-LIMIT"),
                    C("cash_limit", "PA-CASH-LIMIT"),
                    C("credit_balance", "PA-CREDIT-BALANCE"),
                    C("cash_balance", "PA-CASH-BALANCE"),
                    C("approved_auth_cnt", "PA-APPROVED-AUTH-CNT"),
                    C("declined_auth_cnt", "PA-DECLINED-AUTH-CNT"),
                    C("approved_auth_amt", "PA-APPROVED-AUTH-AMT"),
                    C("declined_auth_amt", "PA-DECLINED-AUTH-AMT"),
                ),
                table="pending_auth_summary",
                copybook=PAUTH_CPY / "CIPAUSMY.cpy",
                discriminator="PAUTSUM0",
                skip_blank="PA-ACCT-ID",  # the unload ends with an all-spaces root segment
            ),
            Output(
                "pending_auth_detail",
                (
                    C("acct_id", "@parent_acct_id"),
                    C("auth_date_9c", "PA-AUTH-DATE-9C"),
                    C("auth_time_9c", "PA-AUTH-TIME-9C"),
                    C("auth_orig_date", "PA-AUTH-ORIG-DATE", t.text),
                    C("auth_orig_time", "PA-AUTH-ORIG-TIME", t.text),
                    C("card_num", "PA-CARD-NUM", t.text_nn),
                    C("auth_type", "PA-AUTH-TYPE", t.text),
                    C("card_expiry_date", "PA-CARD-EXPIRY-DATE", t.text),
                    C("message_type", "PA-MESSAGE-TYPE", t.text),
                    C("message_source", "PA-MESSAGE-SOURCE", t.text),
                    C("auth_id_code", "PA-AUTH-ID-CODE", t.text),
                    C("auth_resp_code", "PA-AUTH-RESP-CODE", t.text),
                    C("auth_resp_reason", "PA-AUTH-RESP-REASON", t.text),
                    C("processing_code", "PA-PROCESSING-CODE"),
                    C("transaction_amt", "PA-TRANSACTION-AMT"),
                    C("approved_amt", "PA-APPROVED-AMT"),
                    C("merchant_category_code", "PA-MERCHANT-CATAGORY-CODE", t.text),
                    C("acqr_country_code", "PA-ACQR-COUNTRY-CODE", t.text),
                    C("pos_entry_mode", "PA-POS-ENTRY-MODE"),
                    C("merchant_id", "PA-MERCHANT-ID", t.text),
                    C("merchant_name", "PA-MERCHANT-NAME", t.text),
                    C("merchant_city", "PA-MERCHANT-CITY", t.text),
                    C("merchant_state", "PA-MERCHANT-STATE", t.text),
                    C("merchant_zip", "PA-MERCHANT-ZIP", t.text),
                    C("transaction_id", "PA-TRANSACTION-ID", t.text),
                    C("match_status", "PA-MATCH-STATUS", t.text),
                    C("auth_fraud", "PA-AUTH-FRAUD", t.text),
                    C("fraud_rpt_date", "PA-FRAUD-RPT-DATE", t.text),
                ),
                table="pending_auth_detail",
                copybook=PAUTH_CPY / "CIPAUDTY.cpy",
                discriminator="PAUTDTL1",
            ),
        ),
        reader="ims",
    )
)
