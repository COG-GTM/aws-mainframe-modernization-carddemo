/** CVCUS01Y and CUSTREC: customer master record (RECLN 500). */
import { alnum, defineRecord, type Fields, num, type RecordOf } from '../record/layout.js';

function customerFields<D extends string>(dobKey: D, dobName: string) {
  return {
    custId: num('CUST-ID', '9(09)'),
    custFirstName: alnum('CUST-FIRST-NAME', 'X(25)'),
    custMiddleName: alnum('CUST-MIDDLE-NAME', 'X(25)'),
    custLastName: alnum('CUST-LAST-NAME', 'X(25)'),
    custAddrLine1: alnum('CUST-ADDR-LINE-1', 'X(50)'),
    custAddrLine2: alnum('CUST-ADDR-LINE-2', 'X(50)'),
    custAddrLine3: alnum('CUST-ADDR-LINE-3', 'X(50)'),
    custAddrStateCd: alnum('CUST-ADDR-STATE-CD', 'X(02)'),
    custAddrCountryCd: alnum('CUST-ADDR-COUNTRY-CD', 'X(03)'),
    custAddrZip: alnum('CUST-ADDR-ZIP', 'X(10)'),
    custPhoneNum1: alnum('CUST-PHONE-NUM-1', 'X(15)'),
    custPhoneNum2: alnum('CUST-PHONE-NUM-2', 'X(15)'),
    custSsn: num('CUST-SSN', '9(09)'),
    custGovtIssuedId: alnum('CUST-GOVT-ISSUED-ID', 'X(20)'),
    ...({ [dobKey]: alnum(dobName, 'X(10)') } as { [K in D]: ReturnType<typeof alnum> }),
    custEftAccountId: alnum('CUST-EFT-ACCOUNT-ID', 'X(10)'),
    custPriCardHolderInd: alnum('CUST-PRI-CARD-HOLDER-IND', 'X(01)'),
    custFicoCreditScore: num('CUST-FICO-CREDIT-SCORE', '9(03)'),
    filler: alnum('FILLER', 'X(168)'),
  } satisfies Fields;
}

/** CVCUS01Y CUSTOMER-RECORD. */
export const CustomerRecordLayout = defineRecord('CUSTOMER-RECORD', customerFields('custDobYyyyMmDd', 'CUST-DOB-YYYY-MM-DD'));
export type CustomerRecord = RecordOf<typeof CustomerRecordLayout.root.fields>;

/** CUSTREC CUSTOMER-RECORD (identical layout; DOB field is named CUST-DOB-YYYYMMDD). */
export const CustrecLayout = defineRecord('CUSTOMER-RECORD', customerFields('custDobYyyymmdd', 'CUST-DOB-YYYYMMDD'));
export type Custrec = RecordOf<typeof CustrecLayout.root.fields>;
