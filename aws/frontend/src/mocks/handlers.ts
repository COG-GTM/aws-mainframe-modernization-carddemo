import { http, HttpResponse, type DefaultBodyType, type StrictRequest } from 'msw';
import type {
  Account,
  AccountUpdate,
  CardUpdate,
  MenuOption,
  NewTransaction,
  NewUser,
  Page,
  ReportRequest,
  Role,
  SignonRequest,
  UserUpdate,
} from '../api/types';
import { decodeJwt, isExpired, type JwtClaims } from '../auth/jwt';
import { db, type StoredUser } from './db';

const API = '*/api/v1';

function error(status: number, errorCode: string, message: string, legacyProgram: string, field?: string) {
  return HttpResponse.json(
    {
      errorCode,
      message,
      fieldErrors: field ? [{ field, message }] : undefined,
      legacyProgram,
      timestamp: new Date().toISOString(),
    },
    { status },
  );
}

const b64url = (s: string) => btoa(unescape(encodeURIComponent(s))).replace(/=+$/, '').replace(/\+/g, '-').replace(/\//g, '_');

function issueToken(user: StoredUser, ttlMinutes = 60): { token: string; expiresAt: string } {
  const exp = Math.floor(Date.now() / 1000) + ttlMinutes * 60;
  const role: Role = user.userType === 'A' ? 'ADMIN' : 'USER';
  const header = b64url(JSON.stringify({ alg: 'HS256', typ: 'JWT' }));
  const payload = b64url(JSON.stringify({ sub: user.userId, role, name: `${user.firstName} ${user.lastName}`, exp }));
  return { token: `${header}.${payload}.mock-signature`, expiresAt: new Date(exp * 1000).toISOString() };
}

function auth(request: StrictRequest<DefaultBodyType> | Request, role?: Role): JwtClaims | Response {
  const header = request.headers.get('Authorization') ?? '';
  const claims = header.startsWith('Bearer ') ? decodeJwt(header.slice(7)) : null;
  if (!claims || isExpired(claims)) return error(401, 'UNAUTHENTICATED', 'Please sign on to CardDemo ...', 'COSGN00C');
  if (role === 'ADMIN' && claims.role !== 'ADMIN') {
    return error(403, 'FORBIDDEN', 'No access - Admin Only option... ', 'COMEN01C');
  }
  return claims;
}

function paginate<T>(rows: T[], keyOf: (r: T) => string, url: URL, defaultSize: number): Page<T> {
  const startKey = url.searchParams.get('startKey') ?? '';
  const direction = url.searchParams.get('direction') === 'prev' ? 'prev' : 'next';
  const pageSize = Math.min(Number(url.searchParams.get('pageSize') ?? defaultSize) || defaultSize, 100);
  let items: T[];
  if (direction === 'next') {
    const from = startKey ? rows.filter((r) => keyOf(r) > startKey) : rows;
    items = from.slice(0, pageSize);
  } else {
    const before = startKey ? rows.filter((r) => keyOf(r) < startKey) : rows;
    items = before.slice(Math.max(0, before.length - pageSize));
  }
  const firstKey = items.length ? keyOf(items[0]) : null;
  const lastKey = items.length ? keyOf(items[items.length - 1]) : null;
  return {
    items,
    firstKey,
    lastKey,
    hasNext: lastKey !== null && rows.some((r) => keyOf(r) > lastKey),
    hasPrev: firstKey !== null && rows.some((r) => keyOf(r) < firstKey),
  };
}

const MAIN_MENU: MenuOption[] = [
  ['Account View', 'COACTVWC', '/accounts/view'],
  ['Account Update', 'COACTUPC', '/accounts/update'],
  ['Credit Card List', 'COCRDLIC', '/cards'],
  ['Credit Card View', 'COCRDSLC', '/cards/view'],
  ['Credit Card Update', 'COCRDUPC', '/cards/update'],
  ['Transaction List', 'COTRN00C', '/transactions'],
  ['Transaction View', 'COTRN01C', '/transactions/view'],
  ['Transaction Add', 'COTRN02C', '/transactions/new'],
  ['Transaction Reports', 'CORPT00C', '/reports'],
  ['Bill Payment', 'COBIL00C', '/bill-payment'],
  ['Pending Authorization View', 'COPAUS0C', '/authorizations'],
].map(([name, legacyProgram, route], i) => ({
  number: i + 1,
  name,
  legacyProgram,
  route,
  adminOnly: false,
  installed: legacyProgram !== 'COPAUS0C',
}));

const ADMIN_MENU: MenuOption[] = [
  ['User List (Security)', 'COUSR00C', '/admin/users'],
  ['User Add (Security)', 'COUSR01C', '/admin/users/new'],
  ['User Update (Security)', 'COUSR02C', '/admin/users/:userId/edit'],
  ['User Delete (Security)', 'COUSR03C', '/admin/users/:userId/delete'],
  ['Transaction Type List/Update (Db2)', 'COTRTLIC', '/admin/transaction-types'],
  ['Transaction Type Maintenance (Db2)', 'COTRTUPC', '/admin/transaction-types/maintain'],
].map(([name, legacyProgram, route], i) => ({
  number: i + 1,
  name,
  legacyProgram,
  route,
  adminOnly: true,
  installed: !legacyProgram.startsWith('COTRT'),
}));

const isDigits = (v: string, len?: number) => (len ? new RegExp(`^\\d{${len}}$`) : /^\d+$/).test(v);

function accountView(acctIdRaw: string, program: string): Account | Response {
  if (!acctIdRaw.trim()) return error(400, 'VALIDATION_ERROR', 'Account number not provided', program, 'acctId');
  if (!isDigits(acctIdRaw) || Number(acctIdRaw) === 0 || acctIdRaw.length > 11) {
    return error(400, 'VALIDATION_ERROR', 'Account number must be a non zero 11 digit number', program, 'acctId');
  }
  const acctId = Number(acctIdRaw);
  const xref = db.xrefs.find((x) => x.acctId === acctId);
  if (!xref) return error(404, 'NOT_FOUND', 'Did not find this account in account card xref file', program);
  const account = db.accounts.find((a) => a.acctId === acctId);
  if (!account) return error(404, 'NOT_FOUND', 'Did not find this account in account master file', program);
  const customer = db.customers.find((c) => c.custId === xref.custId);
  if (!customer) return error(404, 'NOT_FOUND', 'Did not find associated customer in master file', program);
  return { ...account, customer: { ...customer } };
}

function nextTranId(): string {
  const max = db.transactions.reduce((m, t) => (t.tranId > m ? t.tranId : m), '0');
  return String(BigInt(max) + 1n).padStart(16, '0');
}

function monthRange(now: Date, type: 'MONTHLY' | 'YEARLY'): [string, string] {
  const y = now.getFullYear();
  const pad = (n: number) => String(n).padStart(2, '0');
  if (type === 'YEARLY') return [`${y}-01-01`, `${y}-12-31`];
  const m = now.getMonth() + 1;
  const last = new Date(y, m, 0).getDate();
  return [`${y}-${pad(m)}-01`, `${y}-${pad(m)}-${pad(last)}`];
}

export const handlers = [
  // §3 Signon — COSGN00C
  http.post(`${API}/auth/signon`, async ({ request }) => {
    const body = (await request.json()) as Partial<SignonRequest>;
    const userId = (body.userId ?? '').trim().toUpperCase();
    const password = (body.password ?? '').trim().toUpperCase();
    if (!userId) return error(400, 'VALIDATION_ERROR', 'Please enter User ID ...', 'COSGN00C', 'userId');
    if (!password) return error(400, 'VALIDATION_ERROR', 'Please enter Password ...', 'COSGN00C', 'password');
    const user = db.users.find((u) => u.userId === userId);
    if (!user) return error(401, 'INVALID_CREDENTIALS', 'User not found. Try again ...', 'COSGN00C');
    if (user.password.toUpperCase() !== password) {
      return error(401, 'INVALID_CREDENTIALS', 'Wrong Password. Try again ...', 'COSGN00C');
    }
    const role: Role = user.userType === 'A' ? 'ADMIN' : 'USER';
    return HttpResponse.json({
      ...issueToken(user),
      tokenType: 'Bearer',
      userId: user.userId,
      firstName: user.firstName,
      lastName: user.lastName,
      role,
      nextRoute: role === 'ADMIN' ? '/admin' : '/menu',
    });
  }),

  // §4 Menus
  http.get(`${API}/menus/main`, ({ request }) => {
    const claims = auth(request);
    if (claims instanceof Response) return claims;
    return HttpResponse.json({ options: MAIN_MENU });
  }),
  http.get(`${API}/menus/admin`, ({ request }) => {
    const claims = auth(request, 'ADMIN');
    if (claims instanceof Response) return claims;
    return HttpResponse.json({ options: ADMIN_MENU });
  }),

  // §5 Accounts — COACTVWC / COACTUPC
  http.get(`${API}/accounts/:acctId`, ({ request, params }) => {
    const claims = auth(request);
    if (claims instanceof Response) return claims;
    const result = accountView(String(params.acctId), 'COACTVWC');
    return result instanceof Response ? result : HttpResponse.json(result);
  }),
  http.put(`${API}/accounts/:acctId`, async ({ request, params }) => {
    const claims = auth(request);
    if (claims instanceof Response) return claims;
    const current = accountView(String(params.acctId), 'COACTUPC');
    if (current instanceof Response) return current;
    const body = (await request.json()) as AccountUpdate;
    if (body.version !== current.version || body.customer?.version !== current.customer.version) {
      return error(409, 'CONCURRENT_UPDATE', 'Record changed by some one else. Please review', 'COACTUPC');
    }
    if (!['Y', 'N'].includes(body.activeStatus)) {
      return error(400, 'VALIDATION_ERROR', 'Account Active Status must be Y or N', 'COACTUPC', 'activeStatus');
    }
    const { customer: custBody, ...acctBody } = body;
    const acct = db.accounts.find((a) => a.acctId === current.acctId)!;
    const cust = db.customers.find((c) => c.custId === current.customer.custId)!;
    const acctChanged = (Object.keys(acctBody) as (keyof typeof acctBody)[]).some(
      (k) => k !== 'version' && String(acctBody[k]) !== String(acct[k as keyof typeof acct]),
    );
    const custChanged = (Object.keys(custBody) as (keyof typeof custBody)[]).some(
      (k) => k !== 'version' && String(custBody[k]) !== String(cust[k]),
    );
    if (!acctChanged && !custChanged) {
      return error(422, 'BUSINESS_RULE', 'No change detected with respect to values fetched.', 'COACTUPC');
    }
    Object.assign(acct, { ...acctBody, addrZip: acctBody.addrZip ?? acct.addrZip, version: acct.version + 1 });
    Object.assign(cust, { ...custBody, version: cust.version + 1 });
    return HttpResponse.json({ ...acct, customer: { ...cust } });
  }),

  // §6 Cards — COCRDLIC / COCRDSLC / COCRDUPC
  http.get(`${API}/cards`, ({ request }) => {
    const claims = auth(request);
    if (claims instanceof Response) return claims;
    const url = new URL(request.url);
    const acctId = url.searchParams.get('acctId') ?? '';
    const cardNum = url.searchParams.get('cardNum') ?? '';
    if (acctId && !isDigits(acctId, 11)) {
      return error(400, 'VALIDATION_ERROR', 'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER', 'COCRDLIC', 'acctId');
    }
    if (cardNum && !isDigits(cardNum, 16)) {
      return error(400, 'VALIDATION_ERROR', 'CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER', 'COCRDLIC', 'cardNum');
    }
    const rows = db.cards
      .filter((c) => (!acctId || c.acctId === Number(acctId)) && (!cardNum || c.cardNum === cardNum))
      .sort((a, b) => a.cardNum.localeCompare(b.cardNum))
      .map(({ cardNum: n, acctId: a, activeStatus }) => ({ cardNum: n, acctId: a, activeStatus }));
    return HttpResponse.json(paginate(rows, (r) => r.cardNum, url, 7));
  }),
  http.get(`${API}/cards/:cardNum`, ({ request, params }) => {
    const claims = auth(request);
    if (claims instanceof Response) return claims;
    const cardNum = String(params.cardNum);
    const acctId = new URL(request.url).searchParams.get('acctId');
    if (!isDigits(cardNum, 16)) {
      return error(400, 'VALIDATION_ERROR', 'Card number if supplied must be a 16 digit number', 'COCRDSLC', 'cardNum');
    }
    if (acctId && (!isDigits(acctId, 11) || Number(acctId) === 0)) {
      return error(400, 'VALIDATION_ERROR', 'Account number must be a non zero 11 digit number', 'COCRDSLC', 'acctId');
    }
    const card = db.cards.find((c) => c.cardNum === cardNum);
    if (!card) return error(404, 'NOT_FOUND', 'Did not find cards for this search condition', 'COCRDSLC');
    if (acctId && card.acctId !== Number(acctId)) {
      return error(404, 'NOT_FOUND', 'Did not find this account in cards database', 'COCRDSLC');
    }
    return HttpResponse.json(card);
  }),
  http.put(`${API}/cards/:cardNum`, async ({ request, params }) => {
    const claims = auth(request);
    if (claims instanceof Response) return claims;
    const card = db.cards.find((c) => c.cardNum === String(params.cardNum));
    if (!card) return error(404, 'NOT_FOUND', 'Did not find cards for this search condition', 'COCRDUPC');
    const body = (await request.json()) as CardUpdate;
    if (body.version !== card.version) {
      return error(409, 'CONCURRENT_UPDATE', 'Record changed by some one else. Please review', 'COCRDUPC');
    }
    if (!/^[A-Za-z ]+$/.test(body.embossedName ?? '')) {
      return error(400, 'VALIDATION_ERROR', 'Card name can only contain alphabets and spaces', 'COCRDUPC', 'embossedName');
    }
    if (
      body.embossedName === card.embossedName &&
      body.activeStatus === card.activeStatus &&
      body.expirationDate === card.expirationDate
    ) {
      return error(422, 'BUSINESS_RULE', 'No change detected with respect to values fetched.', 'COCRDUPC');
    }
    Object.assign(card, {
      embossedName: body.embossedName,
      activeStatus: body.activeStatus,
      expirationDate: body.expirationDate,
      version: card.version + 1,
    });
    return HttpResponse.json(card);
  }),

  // §7 Transactions — COTRN00C / COTRN01C / COTRN02C
  http.get(`${API}/transactions`, ({ request }) => {
    const claims = auth(request);
    if (claims instanceof Response) return claims;
    const url = new URL(request.url);
    const startKey = url.searchParams.get('startKey');
    if (startKey && !isDigits(startKey)) {
      return error(400, 'VALIDATION_ERROR', 'Tran ID must be Numeric ...', 'COTRN00C', 'startKey');
    }
    const rows = db.transactions.map((t) => ({
      tranId: t.tranId,
      origDate: t.origTs.slice(0, 10),
      description: t.description,
      amt: t.amt,
    }));
    return HttpResponse.json(paginate(rows, (r) => r.tranId, url, 10));
  }),
  http.get(`${API}/transactions/:tranId`, ({ request, params }) => {
    const claims = auth(request);
    if (claims instanceof Response) return claims;
    const tranId = String(params.tranId).trim();
    const tran = db.transactions.find((t) => t.tranId === tranId || (isDigits(tranId) && t.tranId === tranId.padStart(16, '0')));
    if (!tran) return error(404, 'NOT_FOUND', 'Transaction ID NOT found...', 'COTRN01C');
    return HttpResponse.json(tran);
  }),
  http.post(`${API}/transactions`, async ({ request }) => {
    const claims = auth(request);
    if (claims instanceof Response) return claims;
    const body = (await request.json()) as NewTransaction;
    let cardNum: string;
    if (body.acctId !== null && body.acctId !== undefined) {
      const xref = db.xrefs.find((x) => x.acctId === Number(body.acctId));
      if (!xref) return error(404, 'NOT_FOUND', 'Account ID NOT found...', 'COTRN02C', 'acctId');
      cardNum = xref.cardNum;
    } else if (body.cardNum) {
      const xref = db.xrefs.find((x) => x.cardNum === body.cardNum);
      if (!xref) return error(404, 'NOT_FOUND', 'Card Number NOT found...', 'COTRN02C', 'cardNum');
      cardNum = xref.cardNum;
    } else {
      return error(400, 'VALIDATION_ERROR', 'Account or Card Number must be entered...', 'COTRN02C', 'acctId');
    }
    if (!/^-?\d{1,8}\.\d{2}$/.test(body.amt)) {
      return error(400, 'VALIDATION_ERROR', 'Amount should be in format -99999999.99', 'COTRN02C', 'amt');
    }
    const tranId = nextTranId();
    db.transactions.push({
      tranId,
      cardNum,
      typeCd: body.typeCd,
      catCd: body.catCd,
      source: body.source,
      description: body.description,
      amt: body.amt,
      origTs: `${body.origDate} 00:00:00.000000`,
      procTs: `${body.procDate} 00:00:00.000000`,
      merchantId: body.merchantId,
      merchantName: body.merchantName,
      merchantCity: body.merchantCity,
      merchantZip: body.merchantZip,
    });
    return HttpResponse.json(
      { tranId, message: `Transaction added successfully. Your Tran ID is ${tranId}.` },
      { status: 201, headers: { Location: `/api/v1/transactions/${tranId}` } },
    );
  }),

  // §7 Bill payment — COBIL00C
  http.get(`${API}/bill-payments/:acctId`, ({ request, params }) => {
    const claims = auth(request);
    if (claims instanceof Response) return claims;
    const raw = String(params.acctId).trim();
    const account = isDigits(raw) ? db.accounts.find((a) => a.acctId === Number(raw)) : undefined;
    if (!account) return error(404, 'NOT_FOUND', 'Account ID NOT found...', 'COBIL00C', 'acctId');
    return HttpResponse.json({ acctId: account.acctId, currBal: account.currBal });
  }),
  http.post(`${API}/bill-payments`, async ({ request }) => {
    const claims = auth(request);
    if (claims instanceof Response) return claims;
    const { acctId } = (await request.json()) as { acctId?: number };
    if (acctId === undefined || acctId === null) {
      return error(400, 'VALIDATION_ERROR', 'Acct ID can NOT be empty...', 'COBIL00C', 'acctId');
    }
    const account = db.accounts.find((a) => a.acctId === Number(acctId));
    if (!account) return error(404, 'NOT_FOUND', 'Account ID NOT found...', 'COBIL00C', 'acctId');
    if (Number(account.currBal) <= 0) return error(422, 'BUSINESS_RULE', 'You have nothing to pay...', 'COBIL00C');
    const xref = db.xrefs.find((x) => x.acctId === account.acctId);
    if (!xref) return error(500, 'INTERNAL_ERROR', 'Unable to lookup XREF AIX file...', 'COBIL00C');
    const tranId = nextTranId();
    const now = new Date().toISOString().replace('T', ' ').replace('Z', '000').slice(0, 26);
    const amount = Number(account.currBal).toFixed(2);
    db.transactions.push({
      tranId,
      cardNum: xref.cardNum,
      typeCd: '02',
      catCd: 2,
      source: 'POS TERM',
      description: 'BILL PAYMENT - ONLINE',
      amt: amount,
      origTs: now,
      procTs: now,
      merchantId: 999999999,
      merchantName: 'BILL PAYMENT',
      merchantCity: 'N/A',
      merchantZip: 'N/A',
    });
    account.currBal = '0.00';
    account.version += 1;
    return HttpResponse.json(
      { tranId, amount, message: `Payment successful. Your Transaction ID is ${tranId}.` },
      { status: 201 },
    );
  }),

  // §7 Reports — CORPT00C
  http.post(`${API}/reports/transactions`, async ({ request }) => {
    const claims = auth(request);
    if (claims instanceof Response) return claims;
    const body = (await request.json()) as Partial<ReportRequest>;
    if (!body.reportType || !['MONTHLY', 'YEARLY', 'CUSTOM'].includes(body.reportType)) {
      return error(400, 'VALIDATION_ERROR', 'Select a report type to print report...', 'CORPT00C', 'reportType');
    }
    let startDate: string;
    let endDate: string;
    if (body.reportType === 'CUSTOM') {
      if (!body.startDate) return error(400, 'VALIDATION_ERROR', 'Start Date - Not a valid date...', 'CORPT00C', 'startDate');
      if (!body.endDate) return error(400, 'VALIDATION_ERROR', 'End Date - Not a valid date...', 'CORPT00C', 'endDate');
      [startDate, endDate] = [body.startDate, body.endDate];
    } else {
      [startDate, endDate] = monthRange(new Date(), body.reportType);
    }
    const requestId = crypto.randomUUID();
    db.reports.set(requestId, { endDate, polls: 0 });
    const name = { MONTHLY: 'Monthly', YEARLY: 'Yearly', CUSTOM: 'Custom' }[body.reportType];
    return HttpResponse.json(
      { requestId, reportType: body.reportType, startDate, endDate, message: `${name} report submitted for printing ...` },
      { status: 202 },
    );
  }),
  http.get(`${API}/reports/transactions/:requestId`, ({ request, params }) => {
    const claims = auth(request);
    if (claims instanceof Response) return claims;
    const requestId = String(params.requestId);
    const report = db.reports.get(requestId);
    if (!report) return HttpResponse.json({ status: 'SUBMITTED' });
    report.polls += 1;
    if (report.polls < 2) return HttpResponse.json({ status: 'RUNNING' });
    return HttpResponse.json({ status: 'SUCCEEDED', reportS3Key: `reports/tranrept/${report.endDate}/${requestId}.txt` });
  }),

  // §8 User administration — COUSR00C-03C
  http.get(`${API}/users`, ({ request }) => {
    const claims = auth(request, 'ADMIN');
    if (claims instanceof Response) return claims;
    const rows = db.users.map(({ userId, firstName, lastName, userType }) => ({ userId, firstName, lastName, userType }));
    return HttpResponse.json(paginate(rows, (r) => r.userId, new URL(request.url), 10));
  }),
  http.get(`${API}/users/:userId`, ({ request, params }) => {
    const claims = auth(request, 'ADMIN');
    if (claims instanceof Response) return claims;
    const user = db.users.find((u) => u.userId === String(params.userId).toUpperCase());
    if (!user) return error(404, 'NOT_FOUND', 'User ID NOT found...', 'COUSR02C', 'userId');
    const { password: _password, ...safe } = user;
    void _password;
    return HttpResponse.json(safe);
  }),
  http.post(`${API}/users`, async ({ request }) => {
    const claims = auth(request, 'ADMIN');
    if (claims instanceof Response) return claims;
    const body = (await request.json()) as NewUser;
    const userId = (body.userId ?? '').trim().toUpperCase();
    if (!userId) return error(400, 'VALIDATION_ERROR', 'User ID can NOT be empty...', 'COUSR01C', 'userId');
    if (db.users.some((u) => u.userId === userId)) {
      return error(409, 'DUPLICATE', 'User ID already exist...', 'COUSR01C', 'userId');
    }
    const user: StoredUser = { ...body, userId, version: 0 };
    db.users.push(user);
    db.users.sort((a, b) => a.userId.localeCompare(b.userId));
    const { password: _password, ...safe } = user;
    void _password;
    return HttpResponse.json(safe, { status: 201, headers: { Location: `/api/v1/users/${userId}` } });
  }),
  http.put(`${API}/users/:userId`, async ({ request, params }) => {
    const claims = auth(request, 'ADMIN');
    if (claims instanceof Response) return claims;
    const user = db.users.find((u) => u.userId === String(params.userId).toUpperCase());
    if (!user) return error(404, 'NOT_FOUND', 'User ID NOT found...', 'COUSR02C', 'userId');
    const body = (await request.json()) as UserUpdate;
    if (body.version !== user.version) {
      return error(409, 'CONCURRENT_UPDATE', 'Record changed by some one else. Please review', 'COUSR02C');
    }
    const changed =
      body.firstName !== user.firstName ||
      body.lastName !== user.lastName ||
      body.userType !== user.userType ||
      (body.password !== undefined && body.password !== user.password);
    if (!changed) return error(422, 'BUSINESS_RULE', 'Please modify to update ...', 'COUSR02C');
    Object.assign(user, {
      firstName: body.firstName,
      lastName: body.lastName,
      userType: body.userType,
      password: body.password ?? user.password,
      version: user.version + 1,
    });
    const { password: _password, ...safe } = user;
    void _password;
    return HttpResponse.json(safe);
  }),
  http.delete(`${API}/users/:userId`, ({ request, params }) => {
    const claims = auth(request, 'ADMIN');
    if (claims instanceof Response) return claims;
    const idx = db.users.findIndex((u) => u.userId === String(params.userId).toUpperCase());
    if (idx < 0) return error(404, 'NOT_FOUND', 'User ID NOT found...', 'COUSR03C', 'userId');
    db.users.splice(idx, 1);
    return new HttpResponse(null, { status: 204 });
  }),
];
