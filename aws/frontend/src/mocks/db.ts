import type { Account, Card, Customer, Transaction, User } from '../api/types';
import seed from './seed.json';

export interface StoredAccount extends Omit<Account, 'customer'> {
  addrZip: string;
}
export interface Xref {
  cardNum: string;
  custId: number;
  acctId: number;
}
export interface StoredUser extends User {
  password: string;
}

export interface MockDb {
  accounts: StoredAccount[];
  customers: Customer[];
  cards: Card[];
  xrefs: Xref[];
  transactions: Transaction[];
  users: StoredUser[];
  reports: Map<string, { endDate: string; polls: number }>;
}

function fresh(): MockDb {
  const clone = structuredClone(seed) as Omit<MockDb, 'reports'>;
  return {
    ...clone,
    transactions: [...clone.transactions].sort((a, b) => a.tranId.localeCompare(b.tranId)),
    users: [...clone.users].sort((a, b) => a.userId.localeCompare(b.userId)),
    reports: new Map(),
  };
}

export let db: MockDb = fresh();

export function resetDb(): void {
  db = fresh();
}
