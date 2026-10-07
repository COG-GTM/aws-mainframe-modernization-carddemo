/** Shapes of the CardDemo REST API (`/v3/api-docs`, com.carddemo.web.*). */

export type Role = 'ADMIN' | 'USER';

export interface ScreenHeader {
  title01: string;
  title02: string;
  tranId: string;
  programName: string;
  currentDate: string;
  currentTime: string;
  applId: string;
  sysId: string;
}

/** COMMAREA replacement (ADR-0017). */
export interface NavigationContext {
  fromTranId: string | null;
  fromProgram: string | null;
  toTranId: string | null;
  toProgram: string | null;
  pgmContext: 'ENTER' | 'REENTER' | null;
  custId: number | null;
  acctId: number | null;
  cardNum: string | null;
}

/** RFC 7807 body with the CardDemo extensions (ADR-0019). */
export interface ApiProblem {
  code: string;
  field: string | null;
  message: string;
  status: number;
  title?: string;
  detail?: string;
  instance?: string;
  invalidFields?: string[];
  toProgram?: string;
}

export interface LoginResponse {
  token: string;
  tokenType: string;
  expiresAt: string;
  userId: string;
  role: Role;
  userType: string;
  targetMenu: string;
  targetMenuUrl: string;
  navigation: NavigationContext;
}

export interface SignOnScreen {
  header: ScreenHeader;
  message: string;
  cursor: string;
}

export interface MenuOptionView {
  number: number;
  label: string;
  name: string;
  programId: string;
  adminOnly: boolean;
}

export type MessageColor = 'DEFAULT' | 'RED' | 'GREEN';

export interface MenuScreen {
  header: ScreenHeader;
  menu: 'main' | 'admin';
  programId: string;
  tranId: string;
  mapset: string;
  map: string;
  options: MenuOptionView[];
  optionLines: string[];
  message: string;
}

export interface MenuSelectionResponse {
  option: string;
  navigation: NavigationContext;
  message: string;
  messageColor: MessageColor;
}

export interface DateInput {
  year: string;
  month: string;
  day: string;
}

export interface PhoneInput {
  areaCode: string;
  prefix: string;
  lineNumber: string;
}

export interface SsnInput {
  part1: string;
  part2: string;
  part3: string;
}

export interface AccountUpdateRequest {
  accountVersion: number;
  customerVersion: number;
  confirm: boolean;
  activeStatus: string;
  openDate: DateInput;
  creditLimit: string;
  expiryDate: DateInput;
  cashCreditLimit: string;
  reissueDate: DateInput;
  currentBalance: string;
  currentCycleCredit: string;
  currentCycleDebit: string;
  groupId: string;
  ssn: SsnInput;
  dateOfBirth: DateInput;
  ficoScore: string;
  firstName: string;
  middleName: string;
  lastName: string;
  addressLine1: string;
  addressLine2: string;
  state: string;
  zip: string;
  city: string;
  phone1: PhoneInput;
  phone2: PhoneInput;
  governmentId: string;
  eftAccountId: string;
  primaryCardHolder: string;
}

export interface AccountViewScreen {
  header: ScreenHeader;
  infoMessage: string;
  message: string;
  acctId: string;
  activeStatus: string;
  openDate: string;
  expirationDate: string;
  reissueDate: string;
  creditLimit: number;
  cashCreditLimit: number;
  currentBalance: number;
  currentCycleCredit: number;
  currentCycleDebit: number;
  groupId: string;
  custId: string;
  ssn: string;
  ficoScore: string;
  dateOfBirth: string;
  firstName: string;
  middleName: string;
  lastName: string;
  addressLine1: string;
  addressLine2: string;
  city: string;
  state: string;
  zip: string;
  country: string;
  phone1: string;
  phone2: string;
  governmentId: string;
  eftAccountId: string;
  primaryCardHolder: string;
  cardNum: string;
  cardNumbers: string[];
  accountVersion: number;
  customerVersion: number;
  updateForm: AccountUpdateRequest;
  exit: NavigationContext;
}

export type UpdateState = 'SHOW' | 'VALIDATED' | 'COMMITTED';

export interface AccountUpdateResponse {
  header: ScreenHeader;
  state: UpdateState;
  updated: boolean;
  infoMessage: string;
  message: string;
  account: AccountViewScreen;
}

export interface CardListRow {
  row: number;
  accountId: string;
  cardNumber: string;
  activeStatus: string;
  cardRef: string;
}

export interface CardListScreen {
  header: ScreenHeader;
  accountId: string | null;
  cardNumber: string | null;
  pageSize: number;
  rows: CardListRow[];
  hasPreviousPage: boolean;
  hasNextPage: boolean;
  previousPage: string | null;
  nextPage: string | null;
  infoMessage: string;
  message: string;
  exit: NavigationContext;
}

export interface CardSelectionResponse {
  header: ScreenHeader;
  navigation: NavigationContext;
  cardRef: string;
  next: string;
  infoMessage: string;
}

export interface CardUpdateRequest {
  accountId: string;
  version: number;
  confirm: boolean;
  embossedName: string;
  activeStatus: string;
  expiryMonth: string;
  expiryYear: string;
}

export interface CardDetailScreen {
  header: ScreenHeader;
  infoMessage: string;
  message: string;
  accountId: string;
  cardNumber: string;
  cardRef: string;
  embossedName: string;
  expiryMonth: string;
  expiryYear: string;
  activeStatus: string;
  version: number;
  updateForm: CardUpdateRequest;
  exit: NavigationContext;
}

export interface CardUpdateResponse {
  header: ScreenHeader;
  state: UpdateState;
  updated: boolean;
  infoMessage: string;
  message: string;
  card: CardDetailScreen;
}

export interface TransactionListRow {
  row: number;
  tranId: string;
  date: string;
  description: string;
  amount: string;
}

export interface TransactionListScreen {
  header: ScreenHeader;
  pageNumber: number;
  pageSize: number;
  rows: TransactionListRow[];
  hasPreviousPage: boolean;
  hasNextPage: boolean;
  previousPage: string | null;
  nextPage: string | null;
  message: string;
  exit: NavigationContext;
}

export interface TransactionSelectionResponse {
  header: ScreenHeader;
  navigation: NavigationContext;
  tranId: string;
  next: string;
}

export interface TransactionFields {
  tranId: string;
  cardNumber: string;
  typeCode: string;
  categoryCode: string;
  source: string;
  amount: string;
  description: string;
  origTimestamp: string;
  procTimestamp: string;
  merchantId: string;
  merchantName: string;
  merchantCity: string;
  merchantZip: string;
}

export interface TransactionDetailScreen {
  header: ScreenHeader;
  transaction: TransactionFields;
  message: string;
  exit: NavigationContext;
  list: NavigationContext;
}

export interface TransactionAddRequest {
  accountId: string;
  cardNumber: string;
  typeCode: string;
  categoryCode: string;
  source: string;
  description: string;
  amount: string;
  origDate: string;
  procDate: string;
  merchantId: string;
  merchantName: string;
  merchantCity: string;
  merchantZip: string;
  confirm: string;
  copyLast?: boolean;
}

export interface TransactionAddResponse {
  header: ScreenHeader;
  state: 'VALIDATED' | 'ADDED';
  form: TransactionAddRequest;
  transaction: TransactionFields | null;
  message: string;
  exit: NavigationContext;
}

export interface BillPaymentResponse {
  header: ScreenHeader;
  state: 'SHOW' | 'CLEARED' | 'PAID';
  accountId: string;
  currentBalance: string;
  version: number;
  transaction: TransactionFields | null;
  message: string;
  exit: NavigationContext;
}

export interface ReportDateFields {
  month: string;
  day: string;
  year: string;
}

export type ReportStatus = 'QUEUED' | 'RUNNING' | 'COMPLETED' | 'FAILED';

export interface TransactionReportRequest {
  reportType: string;
  startDate: ReportDateFields;
  endDate: ReportDateFields;
  confirm: string;
}

export interface TransactionReportResponse {
  header: ScreenHeader;
  state: 'VALIDATED' | 'CANCELLED' | 'SUBMITTED';
  reportName: string;
  startDate: ReportDateFields;
  endDate: ReportDateFields;
  parmStartDate: string;
  parmEndDate: string;
  executionId: number | null;
  status: ReportStatus | null;
  statusUrl: string | null;
  message: string;
  exit: NavigationContext;
}

export interface JobRun {
  jobExecutionId: number;
  jobName: string;
  status: string;
  returnCode: string;
  readCount: number;
  writeCount: number;
  startTime: string;
  endTime: string;
}

export interface ReportFile {
  fileName: string;
  outputFileId: number;
  recordCount: number;
  sha256: string;
  encoding: string;
  downloadUrl: string;
  lines: string[];
}

export interface ReportExecutionResponse {
  executionId: number;
  jobStream: string;
  reportName: string;
  startDate: string;
  endDate: string;
  runDate: string;
  requestedBy: string;
  status: ReportStatus;
  returnCode: number | null;
  message: string;
  submittedAt: string;
  startedAt: string | null;
  endedAt: string | null;
  jobs: JobRun[];
  report: ReportFile | null;
}

export interface UserListRow {
  row: number;
  userId: string;
  firstName: string;
  lastName: string;
  userType: string;
}

export interface UserListScreen {
  header: ScreenHeader;
  pageNumber: number;
  pageSize: number;
  rows: UserListRow[];
  hasPreviousPage: boolean;
  hasNextPage: boolean;
  previousPage: string | null;
  nextPage: string | null;
  message: string;
  exit: NavigationContext;
}

export interface UserSelectionResponse {
  header: ScreenHeader;
  navigation: NavigationContext;
  userId: string;
  next: string;
}

export interface User {
  userId: string;
  firstName: string;
  lastName: string;
  password: string | null;
  userType: string;
  version: number;
}

export interface UserScreen {
  header: ScreenHeader;
  state: 'SHOW' | 'ADDED' | 'UPDATED' | 'VALIDATED' | 'CANCELLED' | 'DELETED';
  user: User | null;
  message: string;
  exit: NavigationContext;
}

export interface UserAddRequest {
  firstName: string;
  lastName: string;
  userId: string;
  password: string;
  userType: string;
}

export interface UserUpdateRequest {
  firstName: string;
  lastName: string;
  password: string;
  userType: string;
  version: number;
}
