import { apiRequest } from './client';
import type {
  Account,
  AccountUpdate,
  BillBalance,
  BillPaymentResult,
  Card,
  CardSummary,
  CardUpdate,
  CreatedTransaction,
  MenuResponse,
  NewTransaction,
  NewUser,
  Page,
  PageQuery,
  ReportRequest,
  ReportStatus,
  ReportSubmitted,
  SignonRequest,
  SignonResponse,
  Transaction,
  TransactionSummary,
  User,
  UserSummary,
  UserUpdate,
} from './types';

const enc = encodeURIComponent;

export const api = {
  signon: (body: SignonRequest) => apiRequest<SignonResponse>('POST', '/auth/signon', { body }),

  mainMenu: () => apiRequest<MenuResponse>('GET', '/menus/main'),
  adminMenu: () => apiRequest<MenuResponse>('GET', '/menus/admin'),

  getAccount: (acctId: string) => apiRequest<Account>('GET', `/accounts/${enc(acctId)}`),
  updateAccount: (acctId: string, body: AccountUpdate) =>
    apiRequest<Account>('PUT', `/accounts/${enc(acctId)}`, { body }),

  listCards: (filters: { acctId?: string; cardNum?: string }, page: PageQuery) =>
    apiRequest<Page<CardSummary>>('GET', '/cards', { query: { ...filters, ...page } }),
  getCard: (cardNum: string, acctId?: string) =>
    apiRequest<Card>('GET', `/cards/${enc(cardNum)}`, { query: { acctId } }),
  updateCard: (cardNum: string, body: CardUpdate) => apiRequest<Card>('PUT', `/cards/${enc(cardNum)}`, { body }),

  listTransactions: (page: PageQuery) =>
    apiRequest<Page<TransactionSummary>>('GET', '/transactions', { query: { ...page } }),
  getTransaction: (tranId: string) => apiRequest<Transaction>('GET', `/transactions/${enc(tranId)}`),
  addTransaction: (body: NewTransaction) => apiRequest<CreatedTransaction>('POST', '/transactions', { body }),

  getBillBalance: (acctId: string) => apiRequest<BillBalance>('GET', `/bill-payments/${enc(acctId)}`),
  payBill: (acctId: number) => apiRequest<BillPaymentResult>('POST', '/bill-payments', { body: { acctId } }),

  submitReport: (body: ReportRequest) => apiRequest<ReportSubmitted>('POST', '/reports/transactions', { body }),
  reportStatus: (requestId: string) => apiRequest<ReportStatus>('GET', `/reports/transactions/${enc(requestId)}`),

  listUsers: (page: PageQuery) => apiRequest<Page<UserSummary>>('GET', '/users', { query: { ...page } }),
  getUser: (userId: string) => apiRequest<User>('GET', `/users/${enc(userId)}`),
  addUser: (body: NewUser) => apiRequest<User>('POST', '/users', { body }),
  updateUser: (userId: string, body: UserUpdate) => apiRequest<User>('PUT', `/users/${enc(userId)}`, { body }),
  deleteUser: (userId: string) => apiRequest<void>('DELETE', `/users/${enc(userId)}`),
};
