export type Role = 'ADMIN' | 'USER';
export type YesNo = 'Y' | 'N';
export type UserType = 'A' | 'U';

export interface FieldError {
  field: string;
  message: string;
}

export interface ErrorEnvelope {
  errorCode: string;
  message: string;
  fieldErrors?: FieldError[];
  legacyProgram?: string;
  timestamp?: string;
}

export interface Page<T> {
  items: T[];
  firstKey: string | null;
  lastKey: string | null;
  hasNext: boolean;
  hasPrev: boolean;
}

export type Direction = 'next' | 'prev';

export interface PageQuery {
  startKey?: string;
  direction?: Direction;
  pageSize?: number;
}

export interface SignonRequest {
  userId: string;
  password: string;
}

export interface SignonResponse {
  token: string;
  tokenType: 'Bearer';
  expiresAt: string;
  userId: string;
  firstName: string;
  lastName: string;
  role: Role;
  nextRoute: string;
}

export interface MenuOption {
  number: number;
  name: string;
  legacyProgram: string;
  route: string;
  adminOnly: boolean;
  installed: boolean;
}

export interface MenuResponse {
  options: MenuOption[];
}

export interface Customer {
  custId: number;
  firstName: string;
  middleName: string;
  lastName: string;
  addrLine1: string;
  addrLine2: string;
  addrLine3: string;
  addrStateCd: string;
  addrCountryCd: string;
  addrZip: string;
  phoneNum1: string;
  phoneNum2: string;
  ssn: string;
  govtIssuedId: string;
  dob: string;
  eftAccountId: string;
  priCardHolderInd: string;
  ficoCreditScore: number;
  version: number;
}

export interface Account {
  acctId: number;
  activeStatus: string;
  currBal: string;
  creditLimit: string;
  cashCreditLimit: string;
  openDate: string;
  expirationDate: string;
  reissueDate: string;
  currCycCredit: string;
  currCycDebit: string;
  groupId: string;
  addrZip?: string;
  version: number;
  customer: Customer;
}

export type AccountUpdate = Omit<Account, 'acctId' | 'customer'> & {
  customer: Omit<Customer, 'custId'>;
};

export interface CardSummary {
  cardNum: string;
  acctId: number;
  activeStatus: string;
}

export interface Card extends CardSummary {
  cvvCd: string;
  embossedName: string;
  expirationDate: string;
  version: number;
}

export interface CardUpdate {
  acctId: number;
  embossedName: string;
  activeStatus: string;
  expirationDate: string;
  version: number;
}

export interface TransactionSummary {
  tranId: string;
  origDate: string;
  description: string;
  amt: string;
}

export interface Transaction {
  tranId: string;
  cardNum: string;
  typeCd: string;
  catCd: number;
  source: string;
  description: string;
  amt: string;
  origTs: string;
  procTs: string;
  merchantId: number;
  merchantName: string;
  merchantCity: string;
  merchantZip: string;
}

export interface NewTransaction {
  acctId: number | null;
  cardNum: string | null;
  typeCd: string;
  catCd: number;
  source: string;
  description: string;
  amt: string;
  origDate: string;
  procDate: string;
  merchantId: number;
  merchantName: string;
  merchantCity: string;
  merchantZip: string;
}

export interface CreatedTransaction {
  tranId: string;
  message: string;
}

export interface BillBalance {
  acctId: number;
  currBal: string;
}

export interface BillPaymentResult {
  tranId: string;
  amount: string;
  message: string;
}

export type ReportType = 'MONTHLY' | 'YEARLY' | 'CUSTOM';

export interface ReportRequest {
  reportType: ReportType;
  startDate?: string;
  endDate?: string;
}

export interface ReportSubmitted {
  requestId: string;
  reportType: ReportType;
  startDate: string;
  endDate: string;
  message: string;
}

export interface ReportStatus {
  status: 'SUBMITTED' | 'RUNNING' | 'SUCCEEDED' | 'FAILED';
  reportS3Key?: string;
}

export interface UserSummary {
  userId: string;
  firstName: string;
  lastName: string;
  userType: UserType;
}

export interface User extends UserSummary {
  version: number;
}

export interface NewUser {
  userId: string;
  firstName: string;
  lastName: string;
  password: string;
  userType: UserType;
}

export interface UserUpdate {
  firstName: string;
  lastName: string;
  password?: string;
  userType: UserType;
  version: number;
}
