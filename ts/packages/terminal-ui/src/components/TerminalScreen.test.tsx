import { cleanup, render, screen as testingScreen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { afterEach, describe, expect, it, vi } from "vitest";

import { createMockScreenClient, handleScreenExchange, initialScreen } from "../api/mock-backend.js";
import { COSGN0A, COTRN0A } from "../generated/index.js";
import { screenText } from "../terminal/screen-model.js";
import { Terminal } from "./Terminal.js";
import { TerminalScreen } from "./TerminalScreen.js";

afterEach(cleanup);

const noop = (): void => undefined;

describe("the signon screen", () => {
  it("puts the BMS literals on the 24x80 grid", () => {
    render(
      <TerminalScreen screen={COSGN0A} values={{}} onFieldChange={noop} onAidKey={noop} />,
    );

    const grid = testingScreen.getByRole("application", { name: "COSGN00 COSGN0A" });
    expect(grid.style.gridTemplateColumns).toBe("repeat(80, 1ch)");
    expect(grid.style.gridTemplateRows).toBe("repeat(24, var(--bms-row-height))");

    const tran = within(grid).getByText("Tran :");
    expect(tran.style.gridRow).toBe("1");
    // POS=(1,1) is the attribute byte, so the literal starts in column 2.
    expect(tran.style.gridColumn).toBe("2 / span 6");

    expect(within(grid).getByText("ENTER=Sign-on F3=Exit").style.gridRow).toBe("24");
  });

  it("renders the unprotected fields as inputs and the dark password field masked", () => {
    render(<TerminalScreen screen={COSGN0A} values={{}} onFieldChange={noop} onAidKey={noop} />);

    const userId = testingScreen.getByLabelText("USERID");
    expect(userId).toHaveProperty("maxLength", 8);
    expect(userId.getAttribute("type")).toBe("text");
    expect(testingScreen.getByLabelText("PASSWD").getAttribute("type")).toBe("password");
    // The protected header fields are text, not inputs.
    expect(testingScreen.queryByLabelText("TRNNAME")).toBeNull();
  });

  it("starts the cursor in the IC field and tabs on in BMS order", async () => {
    const user = userEvent.setup();
    render(<TerminalScreen screen={COSGN0A} values={{}} onFieldChange={noop} onAidKey={noop} />);

    const userId = testingScreen.getByLabelText("USERID");
    await waitFor(() => {
      expect(document.activeElement).toBe(userId);
    });

    await user.tab();
    expect(document.activeElement).toBe(testingScreen.getByLabelText("PASSWD"));
  });

  it("reports typing and the AID keys the program listens for", async () => {
    const user = userEvent.setup();
    const onFieldChange = vi.fn();
    const onAidKey = vi.fn();
    render(
      <TerminalScreen
        screen={COSGN0A}
        values={{ USERID: "" }}
        onFieldChange={onFieldChange}
        onAidKey={onAidKey}
      />,
    );

    await user.type(testingScreen.getByLabelText("USERID"), "A");
    expect(onFieldChange).toHaveBeenCalledWith("USERID", "A");

    await user.keyboard("{Enter}");
    expect(onAidKey).toHaveBeenCalledWith("ENTER");

    await user.keyboard("{F3}");
    expect(onAidKey).toHaveBeenCalledWith("PF3");

    await user.keyboard("{Escape}");
    expect(onAidKey).toHaveBeenCalledWith("CLEAR");
  });
});

describe("the transaction list screen", () => {
  it("renders the rows the program returned", () => {
    const list = handleScreenExchange({
      program: "COTRN00C",
      screenFields: {},
      aidKey: "ENTER",
      state: { page: 1 },
    });

    render(
      <TerminalScreen
        screen={COTRN0A}
        values={list.screenFields}
        onFieldChange={noop}
        onAidKey={noop}
      />,
    );

    const grid = testingScreen.getByRole("application", { name: "COTRN00 COTRN0A" });
    expect(within(grid).getByText("T000000000000001")).toBeDefined();
    expect(within(grid).getByText("Purchase at merchant 010")).toBeDefined();
    // Ten selection fields, one per listed transaction.
    expect(within(grid).getAllByLabelText(/^SEL00\d\d$/)).toHaveLength(10);
  });

  it("pages forward on PF8 and back to the menu on PF3", async () => {
    const user = userEvent.setup();
    render(<Terminal client={createMockScreenClient()} initial={initialScreen()} />);

    await user.type(testingScreen.getByLabelText("USERID"), "USER0001");
    await user.type(testingScreen.getByLabelText("PASSWD"), "PASSWORD");
    await user.keyboard("{Enter}");

    const option = await testingScreen.findByLabelText("OPTION");
    await user.type(option, "6");
    await user.keyboard("{Enter}");

    await testingScreen.findByText("T000000000000001");
    await user.keyboard("{F8}");
    await testingScreen.findByText("T000000000000011");

    await user.keyboard("{F7}");
    await testingScreen.findByText("T000000000000001");

    await user.keyboard("{F3}");
    await testingScreen.findByText(/Transaction List/);
  });
});

describe("screenText", () => {
  it("lays the signon map out as 24 lines of 80 characters", () => {
    const lines = screenText(COSGN0A, { USERID: "USER0001" });
    expect(lines).toHaveLength(24);
    expect(lines.every((line) => line.length === 80)).toBe(true);
    expect(lines[0]?.slice(1, 7)).toBe("Tran :");
    expect(lines[18]?.includes("User ID     :")).toBe(true);
    expect(lines[18]?.slice(43, 51)).toBe("USER0001");
    // PASSWD is a DRK field, so nothing of it reaches the buffer.
    expect(lines[19]?.slice(43, 51)).toBe("        ");
  });
});
