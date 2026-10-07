import { apiDownload, apiRequest } from './client';
import type {
  AccountUpdateRequest,
  AccountUpdateResponse,
  AccountViewScreen,
  BillPaymentResponse,
  CardDetailScreen,
  CardListScreen,
  CardSelectionResponse,
  CardUpdateRequest,
  CardUpdateResponse,
  LoginResponse,
  MenuScreen,
  MenuSelectionResponse,
  NavigationContext,
  ReportExecutionResponse,
  SignOnScreen,
  TransactionAddRequest,
  TransactionAddResponse,
  TransactionDetailScreen,
  TransactionListScreen,
  TransactionReportRequest,
  TransactionReportResponse,
  TransactionSelectionResponse,
  UserAddRequest,
  UserListScreen,
  UserScreen,
  UserSelectionResponse,
  UserUpdateRequest,
} from './types';

const seg = (value: string) => encodeURIComponent(value);

export interface PageQuery {
  after?: string | null;
  before?: string | null;
}

/** Every call the UI makes; all under /api/v1 (ADR-0022). */
export const api = {
  signOnScreen: () => apiRequest<SignOnScreen>('GET', '/auth/login'),
  login: (userId: string, password: string) =>
    apiRequest<LoginResponse>('POST', '/auth/login', { body: { userId, password } }),
  logout: () => apiRequest<{ message: string }>('POST', '/auth/logout'),

  menu: (menu: 'main' | 'admin') => apiRequest<MenuScreen>('GET', `/menu/${menu}`),
  menuSelect: (menu: 'main' | 'admin', option: string) =>
    apiRequest<MenuSelectionResponse>('POST', `/menu/${menu}/selection`, { body: { option } }),
  menuExit: (menu: 'main' | 'admin') => apiRequest<NavigationContext>('POST', `/menu/${menu}/exit`),

  account: (id: string) => apiRequest<AccountViewScreen>('GET', `/accounts/${seg(id)}`),
  updateAccount: (id: string, body: AccountUpdateRequest) =>
    apiRequest<AccountUpdateResponse>('PUT', `/accounts/${seg(id)}`, { body }),

  cards: (q: { accountId?: string; cardNumber?: string } & PageQuery) =>
    apiRequest<CardListScreen>('GET', '/cards', { query: { ...q, limit: 7 } }),
  selectCard: (accountId: string | undefined, rows: { cardRef: string; action: string }[]) =>
    apiRequest<CardSelectionResponse>('POST', '/cards/selection', { body: { accountId: accountId || undefined, rows } }),
  card: (cardNumberOrRef: string, accountId: string, fromProgram?: string | null) =>
    apiRequest<CardDetailScreen>('GET', `/cards/${seg(cardNumberOrRef)}`, { query: { accountId, fromProgram } }),
  cardByAccount: (accountId: string, fromProgram?: string | null) =>
    apiRequest<CardDetailScreen>('GET', `/cards/by-account/${seg(accountId)}`, { query: { fromProgram } }),
  updateCard: (cardRef: string, body: CardUpdateRequest, fromProgram?: string | null) =>
    apiRequest<CardUpdateResponse>('PUT', `/cards/${seg(cardRef)}`, { body, query: { fromProgram } }),

  transactions: (q: { startTranId?: string; page?: number } & PageQuery) =>
    apiRequest<TransactionListScreen>('GET', '/transactions', { query: { ...q, limit: 10 } }),
  selectTransaction: (rows: { tranId: string; selection: string }[]) =>
    apiRequest<TransactionSelectionResponse>('POST', '/transactions/selection', { body: { rows } }),
  transaction: (tranId: string, fromProgram?: string | null) =>
    apiRequest<TransactionDetailScreen>('GET', `/transactions/${seg(tranId)}`, { query: { fromProgram } }),
  addTransaction: (body: TransactionAddRequest, fromProgram?: string | null) =>
    apiRequest<TransactionAddResponse>('POST', '/transactions', { body, query: { fromProgram } }),
  billPayment: (accountId: string, confirm: string, version: number | null, fromProgram?: string | null) =>
    apiRequest<BillPaymentResponse>('POST', `/accounts/${seg(accountId)}/bill-payment`, {
      body: { confirm, version: version ?? undefined },
      query: { fromProgram },
    }),

  submitReport: (body: TransactionReportRequest) =>
    apiRequest<TransactionReportResponse>('POST', '/reports/transactions', { body }),
  reportExecution: (executionId: number) =>
    apiRequest<ReportExecutionResponse>('GET', `/reports/transactions/${executionId}`),
  reportFile: (executionId: number) => apiDownload(`/reports/transactions/${executionId}/report`),

  users: (q: { startUserId?: string; page?: number } & PageQuery) =>
    apiRequest<UserListScreen>('GET', '/users', { query: { ...q, limit: 10 } }),
  selectUser: (rows: { userId: string; selection: string }[]) =>
    apiRequest<UserSelectionResponse>('POST', '/users/selection', { body: { rows } }),
  addUser: (body: UserAddRequest) => apiRequest<UserScreen>('POST', '/users', { body }),
  user: (id: string, fromProgram?: string | null) =>
    apiRequest<UserScreen>('GET', `/users/${seg(id)}`, { query: { fromProgram } }),
  updateUser: (id: string, body: UserUpdateRequest, fromProgram?: string | null) =>
    apiRequest<UserScreen>('PUT', `/users/${seg(id)}`, { body, query: { fromProgram } }),
  deleteUser: (id: string, confirm: string, version: number | null, fromProgram?: string | null) =>
    apiRequest<UserScreen>('DELETE', `/users/${seg(id)}`, { query: { confirm, version, fromProgram } }),
};
