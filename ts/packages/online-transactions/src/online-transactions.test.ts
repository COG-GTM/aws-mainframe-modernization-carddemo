import { FileStatus } from "@carddemo/vsam";
import { describe, expect, it } from "vitest";

import { handleBillPayment, emptyBillPaymentScreen } from "./bill-payment.js";
import { emptyCommarea } from "./commarea.js";
import { addMessages, billPayMessages, listMessages, reportMessages, viewMessages } from "./messages.js";
import { emptyReportRequestScreen, handleReportRequest } from "./report-request.js";
import { AidKey } from "./screen.js";
import { testContext } from "./test-support.js";
import { emptyTransactionAddScreen, handleTransactionAdd } from "./transaction-add.js";
import { emptyTransactionListScreen, handleTransactionList, PAGE_SIZE } from "./transaction-list.js";
import { emptyTransactionViewScreen, handleTransactionView } from "./transaction-view.js";

const signedOn = () => ({ ...emptyCommarea(), userId: "USER0001", pgmContext: 1 });

describe("COTRN00C transaction list", () => {
  it("fills a page of ten rows and pages forward and back", () => {
    const { context } = testContext();
    const first = handleTransactionList(
      { screen: emptyTransactionListScreen(), aid: AidKey.enter, commarea: signedOn() },
      context,
    );

    expect(first.screen.rows).toHaveLength(PAGE_SIZE);
    expect(first.screen.rows.every((row) => row.tranId.length === 16)).toBe(true);
    expect(first.screen.pageNum).toBe("1");
    expect(first.commarea.listState.nextPageFlag).toBe("Y");

    const second = handleTransactionList(
      { screen: first.screen, aid: AidKey.pf8, commarea: first.commarea },
      context,
    );
    expect(second.screen.pageNum).toBe("2");
    expect(second.screen.rows[0]?.tranId).not.toBe(first.screen.rows[0]?.tranId);

    const back = handleTransactionList(
      { screen: second.screen, aid: AidKey.pf7, commarea: second.commarea },
      context,
    );
    expect(back.screen.pageNum).toBe("1");
    expect(back.screen.rows.map((row) => row.tranId)).toEqual(
      first.screen.rows.map((row) => row.tranId),
    );
  });

  it("reports the top of the page on PF7 from the first page", () => {
    const { context } = testContext();
    const first = handleTransactionList(
      { screen: emptyTransactionListScreen(), aid: AidKey.enter, commarea: signedOn() },
      context,
    );
    const back = handleTransactionList(
      { screen: first.screen, aid: AidKey.pf7, commarea: first.commarea },
      context,
    );
    expect(back.screen.errMsg).toBe(listMessages.alreadyTopOfPage);
  });

  it("rejects a non numeric transaction id filter", () => {
    const { context } = testContext();
    const response = handleTransactionList(
      {
        screen: { ...emptyTransactionListScreen(), tranIdFilter: "ABC" },
        aid: AidKey.enter,
        commarea: signedOn(),
      },
      context,
    );
    expect(response.screen.errMsg).toBe(listMessages.tranIdNotNumeric);
  });

  it("selects a row with S and transfers to COTRN01C", () => {
    const { context } = testContext();
    const first = handleTransactionList(
      { screen: emptyTransactionListScreen(), aid: AidKey.enter, commarea: signedOn() },
      context,
    );
    const rows = [...first.screen.rows];
    const target = rows[2];
    if (target === undefined) {
      throw new Error("expected a third row");
    }
    rows[2] = { ...target, sel: "S" };

    const selected = handleTransactionList(
      { screen: { ...first.screen, rows }, aid: AidKey.enter, commarea: first.commarea },
      context,
    );
    expect(selected.transfer).toBe(true);
    expect(selected.nextProgram).toBe("COTRN01C");
    expect(selected.commarea.listState.trnSelected).toBe(target.tranId);
  });

  it("rejects a selection other than S", () => {
    const { context } = testContext();
    const first = handleTransactionList(
      { screen: emptyTransactionListScreen(), aid: AidKey.enter, commarea: signedOn() },
      context,
    );
    const rows = [...first.screen.rows];
    const target = rows[0];
    if (target === undefined) {
      throw new Error("expected a first row");
    }
    rows[0] = { ...target, sel: "X" };

    const response = handleTransactionList(
      { screen: { ...first.screen, rows }, aid: AidKey.enter, commarea: first.commarea },
      context,
    );
    expect(response.transfer).toBe(false);
    expect(response.screen.errMsg).toBe(listMessages.invalidSelection);
  });
});

describe("COTRN01C transaction view", () => {
  it("shows an existing transaction", () => {
    const { context } = testContext();
    const list = handleTransactionList(
      { screen: emptyTransactionListScreen(), aid: AidKey.enter, commarea: signedOn() },
      context,
    );
    const tranId = list.screen.rows[0]?.tranId ?? "";

    const view = handleTransactionView(
      {
        screen: { ...emptyTransactionViewScreen(), tranIdIn: tranId },
        aid: AidKey.enter,
        commarea: { ...signedOn(), listState: list.commarea.listState },
      },
      context,
    );

    expect(view.screen.errMsg).toBe("");
    expect(view.screen.tranId).toBe(tranId);
    expect(view.screen.cardNum.trim().length).toBeGreaterThan(0);
  });

  it("reports a missing transaction and an empty id", () => {
    const { context } = testContext();
    const missing = handleTransactionView(
      {
        screen: { ...emptyTransactionViewScreen(), tranIdIn: "9999999999999999" },
        aid: AidKey.enter,
        commarea: signedOn(),
      },
      context,
    );
    expect(missing.screen.errMsg).toBe(viewMessages.tranIdNotFound);

    const empty = handleTransactionView(
      { screen: emptyTransactionViewScreen(), aid: AidKey.enter, commarea: signedOn() },
      context,
    );
    expect(empty.screen.errMsg).toBe(viewMessages.tranIdEmpty);
  });

  it("rejects an unsupported key", () => {
    const { context } = testContext();
    const response = handleTransactionView(
      { screen: emptyTransactionViewScreen(), aid: AidKey.pf7, commarea: signedOn() },
      context,
    );
    expect(response.screen.errMsg).toBe("Invalid key pressed. Please see below...");
  });
});

const addScreen = () => ({
  ...emptyTransactionAddScreen(),
  acctId: "00000000011",
  typeCd: "01",
  catCd: "5001",
  source: "POS TERM",
  description: "Coffee",
  amount: "-00000012.34",
  origDate: "2024-05-17",
  procDate: "2024-05-17",
  merchantId: "123456789",
  merchantName: "Corner Store",
  merchantCity: "Springfield",
  merchantZip: "12345",
});

describe("COTRN02C add transaction", () => {
  it("requires an account or card number", () => {
    const { context } = testContext();
    const response = handleTransactionAdd(
      { screen: emptyTransactionAddScreen(), aid: AidKey.enter, commarea: signedOn() },
      context,
    );
    expect(response.screen.errMsg).toBe(addMessages.keyRequired);
  });

  it("rejects a non numeric account id and an unknown account", () => {
    const { context } = testContext();
    const notNumeric = handleTransactionAdd(
      {
        screen: { ...emptyTransactionAddScreen(), acctId: "ABCDEFGHIJK" },
        aid: AidKey.enter,
        commarea: signedOn(),
      },
      context,
    );
    expect(notNumeric.screen.errMsg).toBe(addMessages.acctIdNotNumeric);

    const unknown = handleTransactionAdd(
      {
        screen: { ...emptyTransactionAddScreen(), acctId: "99999999999" },
        aid: AidKey.enter,
        commarea: signedOn(),
      },
      context,
    );
    expect(unknown.screen.errMsg).toBe(addMessages.acctIdNotFound);
  });

  it("edits the data fields in the COBOL order", () => {
    const { context } = testContext();
    const enter = (screen: ReturnType<typeof addScreen>) =>
      handleTransactionAdd({ screen, aid: AidKey.enter, commarea: signedOn() }, context).screen
        .errMsg;

    expect(enter({ ...addScreen(), typeCd: "" })).toBe(addMessages.typeCdEmpty);
    expect(enter({ ...addScreen(), catCd: "" })).toBe(addMessages.catCdEmpty);
    expect(enter({ ...addScreen(), merchantZip: "" })).toBe(addMessages.merchantZipEmpty);
    expect(enter({ ...addScreen(), typeCd: "AB" })).toBe(addMessages.typeCdNotNumeric);
    expect(enter({ ...addScreen(), amount: "12.34" })).toBe(addMessages.amountFormat);
    expect(enter({ ...addScreen(), origDate: "17/05/2024" })).toBe(addMessages.origDateFormat);
    expect(enter({ ...addScreen(), procDate: "2024-13-45" })).toBe(addMessages.procDateInvalid);
    expect(enter({ ...addScreen(), merchantId: "12345678X" })).toBe(
      addMessages.merchantIdNotNumeric,
    );
  });

  it("asks for a confirmation and then writes the transaction", () => {
    const { context } = testContext();
    const confirm = handleTransactionAdd(
      { screen: addScreen(), aid: AidKey.enter, commarea: signedOn() },
      context,
    );
    expect(confirm.screen.errMsg).toBe(addMessages.confirmAdd);
    expect(confirm.screen.cardNum.trim().length).toBe(16);

    const before = context.files.transactions.toArray();
    const added = handleTransactionAdd(
      {
        screen: { ...confirm.screen, confirm: "Y" },
        aid: AidKey.enter,
        commarea: signedOn(),
      },
      context,
    );

    const after = context.files.transactions.toArray();
    expect(after).toHaveLength(before.length + 1);

    const previousId = Number(before[before.length - 1]?.tranId);
    const written = after.find((record) => Number(record.tranId) === previousId + 1);
    expect(written).toBeDefined();
    expect(added.screen.errMsg).toBe(
      `Transaction added successfully.  Your Tran ID is ${written?.tranId ?? ""}.`,
    );
    expect(added.messageColor).toBe("green");
    expect(written?.tranAmt).toBe(-12.34);
    expect(written?.tranCardNum).toBe(confirm.screen.cardNum);
    expect(written?.tranMerchantId).toBe(123456789);
  });

  it("clears the screen on PF4", () => {
    const { context } = testContext();
    const response = handleTransactionAdd(
      { screen: addScreen(), aid: AidKey.pf4, commarea: signedOn() },
      context,
    );
    expect(response.screen).toEqual(emptyTransactionAddScreen());
  });
});

describe("COBIL00C bill payment", () => {
  it("requires an account id", () => {
    const { context } = testContext();
    const response = handleBillPayment(
      { screen: emptyBillPaymentScreen(), aid: AidKey.enter, commarea: signedOn() },
      context,
    );
    expect(response.screen.errMsg).toBe(billPayMessages.acctIdEmpty);
  });

  it("reports an unknown account", () => {
    const { context } = testContext();
    const response = handleBillPayment(
      {
        screen: { ...emptyBillPaymentScreen(), acctId: "99999999999" },
        aid: AidKey.enter,
        commarea: signedOn(),
      },
      context,
    );
    expect(response.screen.errMsg).toBe(billPayMessages.acctIdNotFound);
  });

  it("writes the payment transaction and clears the balance", () => {
    const { context } = testContext();
    const acctId = "00000000011";
    const account = context.files.accounts.read(acctId);
    expect(account.status).toBe(FileStatus.ok);
    const balance = account.record?.acctCurrBal ?? 0;
    expect(balance).toBeGreaterThan(0);

    const prompted = handleBillPayment(
      {
        screen: { ...emptyBillPaymentScreen(), acctId },
        aid: AidKey.enter,
        commarea: signedOn(),
      },
      context,
    );
    expect(prompted.screen.errMsg).toBe(billPayMessages.confirmPayment);
    expect(prompted.screen.currBal).toBe(
      `+${String(Math.trunc(balance)).padStart(10, "0")}.${String(Math.round((balance % 1) * 100)).padStart(2, "0")}`,
    );

    const paid = handleBillPayment(
      {
        screen: { ...prompted.screen, confirm: "Y" },
        aid: AidKey.enter,
        commarea: signedOn(),
      },
      context,
    );

    expect(paid.messageColor).toBe("green");
    expect(paid.screen.errMsg).toMatch(/^Payment successful\. {2}Your Transaction ID is \d{16}\.$/);

    const payment = context.files.transactions
      .toArray()
      .find((record) => record.tranDesc.trimEnd() === "BILL PAYMENT - ONLINE");
    expect(payment).toBeDefined();
    expect(payment?.tranAmt).toBe(balance);
    expect(payment?.tranTypeCd).toBe("02");
    expect(payment?.tranCatCd).toBe(2);
    expect(payment?.tranMerchantId).toBe(999999999);

    expect(context.files.accounts.read(acctId).record?.acctCurrBal).toBe(0);
  });

  it("refuses a payment when there is nothing to pay", () => {
    const { context } = testContext();
    const acctId = "00000000011";
    const account = context.files.accounts.read(acctId).record;
    if (account === undefined) {
      throw new Error("expected the account");
    }
    context.files.accounts.rewrite({ ...account, acctCurrBal: 0 });

    const response = handleBillPayment(
      {
        screen: { ...emptyBillPaymentScreen(), acctId },
        aid: AidKey.enter,
        commarea: signedOn(),
      },
      context,
    );
    expect(response.screen.errMsg).toBe(billPayMessages.nothingToPay);
  });
});

describe("CORPT00C report request", () => {
  it("requires a report type", () => {
    const { context } = testContext();
    const response = handleReportRequest(
      { screen: emptyReportRequestScreen(), aid: AidKey.enter, commarea: signedOn() },
      context,
    );
    expect(response.screen.errMsg).toBe(reportMessages.selectReportType);
  });

  it("validates the custom date range", () => {
    const { context } = testContext();
    const custom = { ...emptyReportRequestScreen(), custom: "S" };
    const enter = (screen: typeof custom) =>
      handleReportRequest({ screen, aid: AidKey.enter, commarea: signedOn() }, context).screen
        .errMsg;

    expect(enter(custom)).toBe(reportMessages.startMonthEmpty);
    expect(enter({ ...custom, startMonth: "05" })).toBe(reportMessages.startDayEmpty);
    expect(
      enter({
        ...custom,
        startMonth: "13",
        startDay: "01",
        startYear: "2024",
        endMonth: "05",
        endDay: "31",
        endYear: "2024",
      }),
    ).toBe(reportMessages.startMonthInvalid);
    expect(
      enter({
        ...custom,
        startMonth: "02",
        startDay: "30",
        startYear: "2024",
        endMonth: "05",
        endDay: "31",
        endYear: "2024",
      }),
    ).toBe(reportMessages.startDateInvalid);
  });

  it("confirms and records the report request", () => {
    const { context, reports } = testContext();
    const screen = {
      ...emptyReportRequestScreen(),
      custom: "S",
      startMonth: "5",
      startDay: "1",
      startYear: "2024",
      endMonth: "5",
      endDay: "31",
      endYear: "2024",
    };

    const prompted = handleReportRequest(
      { screen, aid: AidKey.enter, commarea: signedOn() },
      context,
    );
    expect(prompted.screen.errMsg).toBe("Please confirm to print the Custom report...");
    expect(prompted.screen.startMonth).toBe("05");

    const submitted = handleReportRequest(
      { screen: { ...prompted.screen, confirm: "Y" }, aid: AidKey.enter, commarea: signedOn() },
      context,
    );
    expect(submitted.screen.errMsg).toBe("Custom report submitted for printing ...");
    expect(reports.toArray()).toEqual([
      {
        reportName: "Custom",
        reportType: "custom",
        startDate: "2024-05-01",
        endDate: "2024-05-31",
        userId: "USER0001",
        submittedAt: context.now(),
        jobName: "TRANREPT",
      },
    ]);
  });

  it("rejects an invalid confirmation value", () => {
    const { context } = testContext();
    const response = handleReportRequest(
      {
        screen: { ...emptyReportRequestScreen(), monthly: "S", confirm: "X" },
        aid: AidKey.enter,
        commarea: signedOn(),
      },
      context,
    );
    expect(response.screen.errMsg).toBe('"X" is not a valid value to confirm...');
  });

  it("derives the monthly range from the clock", () => {
    const { context, reports } = testContext();
    handleReportRequest(
      {
        screen: { ...emptyReportRequestScreen(), monthly: "S", confirm: "Y" },
        aid: AidKey.enter,
        commarea: signedOn(),
      },
      context,
    );
    expect(reports.toArray()[0]?.startDate).toBe("2024-05-01");
    expect(reports.toArray()[0]?.endDate).toBe("2024-05-31");
  });
});
