import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import { SCREENS_BY_MAPSET } from "../generated/index.js";
import { joinContinuations, parseBms } from "./parser.js";
import type { BmsField } from "./types.js";

const bmsDirectory = resolve(dirname(fileURLToPath(import.meta.url)), "../../../../../app/bms");

function read(mapset: string): string {
  return readFileSync(join(bmsDirectory, `${mapset}.bms`), "latin1");
}

function field(mapset: string, name: string): BmsField {
  const [screen] = parseBms(read(mapset)).maps;
  const found = screen?.fields.find((candidate) => candidate.name === name);
  if (found === undefined) {
    throw new Error(`${mapset} has no field ${name}`);
  }
  return found;
}

describe("joinContinuations", () => {
  it("folds the continuation lines into one statement", () => {
    const statements = joinContinuations(read("COSGN00"));
    expect(statements[0]?.startsWith("COSGN00 DFHMSD")).toBe(true);
    expect(statements[0]).toContain("TYPE=&&SYSPARM");
    expect(statements.some((statement) => statement.includes("COSGN0A DFHMDI"))).toBe(true);
  });

  it("keeps a literal that is split across columns 71 and 16 intact", () => {
    const [screen] = parseBms(read("COSGN00")).maps;
    const initials = (screen?.fields ?? []).map((candidate) => candidate.initial);
    expect(initials).toContain("This is a Credit Card Demo Application for Mainframe Modernization");
  });
});

describe("parseBms", () => {
  it("reads the signon map COSGN00", () => {
    const mapset = parseBms(read("COSGN00"));
    expect(mapset.name).toBe("COSGN00");
    expect(mapset.maps).toHaveLength(1);

    const [screen] = mapset.maps;
    expect(screen?.map).toBe("COSGN0A");
    expect(screen?.rows).toBe(24);
    expect(screen?.columns).toBe(80);

    expect(field("COSGN00", "USERID")).toMatchObject({
      row: 19,
      column: 43,
      length: 8,
      protected: false,
      autoSkip: false,
      intensity: "normal",
      initialCursor: true,
      fset: true,
      color: "green",
      highlight: "off",
    });

    expect(field("COSGN00", "PASSWD")).toMatchObject({
      row: 20,
      column: 43,
      length: 8,
      protected: false,
      intensity: "dark",
      initial: "________",
    });

    expect(field("COSGN00", "ERRMSG")).toMatchObject({
      row: 23,
      column: 1,
      length: 78,
      protected: true,
      intensity: "bright",
      color: "red",
    });
  });

  it("reads the menu map COMEN01", () => {
    const mapset = parseBms(read("COMEN01"));
    const [screen] = mapset.maps;
    expect(screen?.map).toBe("COMEN1A");

    expect(field("COMEN01", "OPTION")).toMatchObject({
      length: 2,
      protected: false,
      numeric: true,
      initialCursor: true,
    });

    const optionLines = (screen?.fields ?? []).filter((candidate) => candidate.name?.startsWith("OPTN"));
    expect(optionLines).toHaveLength(12);
    expect(optionLines.every((candidate) => candidate.protected)).toBe(true);
  });

  it("reads the transaction list map COTRN00 including its selection column", () => {
    const mapset = parseBms(read("COTRN00"));
    const [screen] = mapset.maps;
    expect(screen?.map).toBe("COTRN0A");

    const selections = (screen?.fields ?? []).filter((candidate) => candidate.name?.startsWith("SEL"));
    expect(selections).toHaveLength(10);
    expect(selections.every((candidate) => !candidate.protected && candidate.length === 1)).toBe(true);

    expect(field("COTRN00", "TRNID01")).toMatchObject({ length: 16, protected: true });
    expect(field("COTRN00", "TRNIDIN").protected).toBe(false);
  });

  it("keeps the PICIN and MUSTFILL operands of COACTVW", () => {
    expect(field("COACTVW", "ACCTSID")).toMatchObject({
      picin: "99999999999",
      mustFill: true,
      highlight: "underline",
    });
  });

  it("defaults a field without ATTRB to an autoskip field at normal intensity", () => {
    const [screen] = parseBms(read("COACTVW")).maps;
    const stopper = (screen?.fields ?? []).find(
      (candidate) => candidate.length === 0 && candidate.row === 5 && candidate.column === 50,
    );
    expect(stopper).toMatchObject({ protected: true, autoSkip: true, intensity: "normal" });
  });
});

describe("the generated screen definitions", () => {
  it("match what the parser produces from app/bms", () => {
    for (const mapset of ["COSGN00", "COMEN01", "COTRN00", "COACTUP"]) {
      const [screen] = parseBms(read(mapset)).maps;
      expect(SCREENS_BY_MAPSET[mapset]).toEqual(screen);
    }
  });
});
