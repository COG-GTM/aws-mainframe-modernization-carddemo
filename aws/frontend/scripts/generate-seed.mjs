// Builds src/mocks/seed.json from the legacy ASCII sample data (app/data/ASCII) and the
// USRSEC seed in app/jcl/DUSRSECJ.jcl so the MSW mock serves the same records as the mainframe.
import { readFileSync, writeFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const appDir = resolve(here, '../../../app');
const out = resolve(here, '../src/mocks/seed.json');
const lookupsOut = resolve(here, '../src/validation/lookups.json');

const lines = (file) =>
  readFileSync(resolve(appDir, file), 'utf8')
    .split(/\r?\n/)
    .filter((l) => l.trim().length > 0);

function cut(line, layout) {
  const rec = {};
  let pos = 0;
  for (const [name, len] of layout) {
    rec[name] = line.substring(pos, pos + len);
    pos += len;
  }
  return rec;
}

const POS = '{ABCDEFGHI';
const NEG = '}JKLMNOPQR';

function zoned(raw, scale = 2) {
  const s = raw.trim();
  const last = s[s.length - 1];
  let digit;
  let sign = 1;
  if (POS.includes(last)) digit = POS.indexOf(last);
  else if (NEG.includes(last)) {
    digit = NEG.indexOf(last);
    sign = -1;
  } else digit = Number(last);
  const digits = s.slice(0, -1) + String(digit);
  const cents = BigInt(digits) * BigInt(sign);
  const neg = cents < 0n;
  const abs = (neg ? -cents : cents).toString().padStart(scale + 1, '0');
  return `${neg ? '-' : ''}${abs.slice(0, -scale)}.${abs.slice(-scale)}`;
}

const t = (s) => s.trim();

const accounts = lines('data/ASCII/acctdata.txt').map((l) => {
  const r = cut(l, [
    ['acctId', 11], ['activeStatus', 1], ['currBal', 12], ['creditLimit', 12], ['cashCreditLimit', 12],
    ['openDate', 10], ['expirationDate', 10], ['reissueDate', 10], ['currCycCredit', 12],
    ['currCycDebit', 12], ['addrZip', 10], ['groupId', 10],
  ]);
  return {
    acctId: Number(r.acctId),
    activeStatus: r.activeStatus,
    currBal: zoned(r.currBal),
    creditLimit: zoned(r.creditLimit),
    cashCreditLimit: zoned(r.cashCreditLimit),
    openDate: r.openDate,
    expirationDate: r.expirationDate,
    reissueDate: r.reissueDate,
    currCycCredit: zoned(r.currCycCredit),
    currCycDebit: zoned(r.currCycDebit),
    // The sample file carries the group id where ACCT-ADDR-ZIP sits in CVACT01Y.
    addrZip: t(r.groupId) ? t(r.addrZip) : '',
    groupId: t(r.groupId) || t(r.addrZip),
    version: 0,
  };
});

const customers = lines('data/ASCII/custdata.txt').map((l) => {
  const r = cut(l, [
    ['custId', 9], ['firstName', 25], ['middleName', 25], ['lastName', 25], ['addrLine1', 50],
    ['addrLine2', 50], ['addrLine3', 50], ['addrStateCd', 2], ['addrCountryCd', 3], ['addrZip', 10],
    ['phoneNum1', 15], ['phoneNum2', 15], ['ssn', 9], ['govtIssuedId', 20], ['dob', 10],
    ['eftAccountId', 10], ['priCardHolderInd', 1], ['ficoCreditScore', 3],
  ]);
  const rec = Object.fromEntries(Object.entries(r).map(([k, v]) => [k, t(v)]));
  return { ...rec, custId: Number(r.custId), ficoCreditScore: Number(r.ficoCreditScore), version: 0 };
});

const cards = lines('data/ASCII/carddata.txt').map((l) => {
  const r = cut(l, [
    ['cardNum', 16], ['acctId', 11], ['cvvCd', 3], ['embossedName', 50], ['expirationDate', 10],
    ['activeStatus', 1],
  ]);
  return {
    cardNum: r.cardNum,
    acctId: Number(r.acctId),
    cvvCd: r.cvvCd,
    embossedName: t(r.embossedName),
    expirationDate: r.expirationDate,
    activeStatus: r.activeStatus,
    version: 0,
  };
});

const xrefs = lines('data/ASCII/cardxref.txt').map((l) => {
  const r = cut(l, [['cardNum', 16], ['custId', 9], ['acctId', 11]]);
  return { cardNum: r.cardNum, custId: Number(r.custId), acctId: Number(r.acctId) };
});

const transactions = lines('data/ASCII/dailytran.txt').map((l) => {
  const r = cut(l, [
    ['tranId', 16], ['typeCd', 2], ['catCd', 4], ['source', 10], ['description', 100], ['amt', 11],
    ['merchantId', 9], ['merchantName', 50], ['merchantCity', 50], ['merchantZip', 10], ['cardNum', 16],
    ['origTs', 26], ['procTs', 26],
  ]);
  return {
    tranId: r.tranId,
    cardNum: r.cardNum,
    typeCd: r.typeCd,
    catCd: Number(r.catCd),
    source: t(r.source),
    description: t(r.description),
    amt: zoned(r.amt),
    origTs: t(r.origTs),
    procTs: t(r.procTs) || t(r.origTs),
    merchantId: Number(r.merchantId),
    merchantName: t(r.merchantName),
    merchantCity: t(r.merchantCity),
    merchantZip: t(r.merchantZip),
  };
});

const jcl = readFileSync(resolve(appDir, 'jcl/DUSRSECJ.jcl'), 'utf8').split(/\r?\n/);
const users = jcl
  .filter((l) => /^(ADMIN|USER)\d{3,4}\S*\s/.test(l) && /PASSWORD[AU]\s*$/.test(l.trimEnd()))
  .map((l) => {
    const r = cut(l, [['userId', 8], ['firstName', 20], ['lastName', 20], ['password', 8], ['userType', 1]]);
    const cap = (s) => t(s).charAt(0) + t(s).slice(1).toLowerCase();
    return {
      userId: t(r.userId),
      firstName: cap(r.firstName),
      lastName: cap(r.lastName),
      password: t(r.password),
      userType: r.userType,
      version: 0,
    };
  });

const lookupSrc = readFileSync(resolve(appDir, 'cpy/CSLKPCDY.cpy'), 'utf8');
function lookup88(name) {
  const start = lookupSrc.indexOf(`88 ${name}`);
  const next = lookupSrc.indexOf(' 88 ', start + 4);
  const block = lookupSrc.slice(start, next === -1 ? undefined : next);
  return [...block.matchAll(/'([^']*)'/g)].map((m) => m[1]);
}
const lookups = {
  generalPurposeAreaCodes: lookup88('VALID-GENERAL-PURP-CODE'),
  usStateCodes: lookup88('VALID-US-STATE-CODE'),
  usStateZip2Combos: lookup88('VALID-US-STATE-ZIP-CD2-COMBO'),
};
writeFileSync(lookupsOut, `${JSON.stringify(lookups)}\n`);

writeFileSync(out, `${JSON.stringify({ accounts, customers, cards, xrefs, transactions, users }, null, 1)}\n`);
console.log(
  `seed: ${accounts.length} accounts, ${customers.length} customers, ${cards.length} cards, ` +
    `${xrefs.length} xrefs, ${transactions.length} transactions, ${users.length} users -> ${out}`,
);
