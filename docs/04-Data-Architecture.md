# CardDemo Data Architecture Documentation

## Overview

The CardDemo application uses IBM VSAM (Virtual Storage Access Method) for data storage, implementing a hierarchical data model with master files, cross-reference files, and transaction files. This document details the complete data architecture, file structures, relationships, and migration considerations for modernization efforts.

---

## VSAM File Structure Overview

### File Organization Types
- **KSDS (Key Sequenced Data Set)**: Primary organization for master data with unique keys
- **AIX (Alternate Index)**: Secondary access paths for flexible data retrieval
- **Sequential Files**: Used for batch processing input/output

### File Access Patterns
- **Random Access**: Direct record retrieval using primary or alternate keys
- **Sequential Access**: Browse operations for reporting and batch processing
- **Dynamic Access**: Combination of random and sequential as needed

---

## Master Data Files

### 1. Account Master Data (ACCTDAT)

**VSAM Dataset**: `AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS`  
**Copybook**: `CVACT01Y.cpy`  
**Record Length**: 300 bytes  
**Key Field**: `ACCT-ID` (PIC 9(11))  

#### Record Structure:
```cobol
01  ACCOUNT-RECORD.
    05  ACCT-ID                           PIC 9(11).
    05  ACCT-ACTIVE-STATUS                PIC X(01).
    05  ACCT-CURR-BAL                     PIC S9(10)V99.
    05  ACCT-CREDIT-LIMIT                 PIC S9(10)V99.
    05  ACCT-CASH-CREDIT-LIMIT            PIC S9(10)V99.
    05  ACCT-OPEN-DATE                    PIC X(10).
    05  ACCT-EXPIRAION-DATE               PIC X(10).
    05  ACCT-REISSUE-DATE                 PIC X(10).
    05  ACCT-CURR-CYC-CREDIT              PIC S9(10)V99.
    05  ACCT-CURR-CYC-DEBIT               PIC S9(10)V99.
    05  ACCT-ADDR-ZIP                     PIC X(10).
    05  ACCT-GROUP-ID                     PIC X(10).
    05  FILLER                            PIC X(178).
```

#### Field Descriptions:
- **ACCT-ID**: Unique 11-digit account identifier
- **ACCT-ACTIVE-STATUS**: 'Y' (Active) or 'N' (Inactive)
- **ACCT-CURR-BAL**: Current account balance (signed, 2 decimal places)
- **ACCT-CREDIT-LIMIT**: Maximum credit limit allowed
- **ACCT-CASH-CREDIT-LIMIT**: Cash advance limit (≤ credit limit)
- **Date Fields**: YYYY-MM-DD format for open, expiration, reissue dates
- **Cycle Amounts**: Current billing cycle credit and debit totals
- **ACCT-ADDR-ZIP**: ZIP code for account address
- **ACCT-GROUP-ID**: Account grouping for interest calculation

#### Business Rules:
- Account ID must be unique across the system
- Cash credit limit cannot exceed credit limit
- Active status controls transaction processing eligibility
- Current balance can be negative (credit balance) or positive (debit balance)
- Cycle amounts reset during monthly processing

---

### 2. Customer Master Data (CUSTDAT)

**VSAM Dataset**: `AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS`  
**Copybook**: `CVCUS01Y.cpy`  
**Record Length**: 500 bytes  
**Key Field**: `CUST-ID` (PIC 9(09))  

#### Record Structure:
```cobol
01  CUSTOMER-RECORD.
    05  CUST-ID                                 PIC 9(09).
    05  CUST-FIRST-NAME                         PIC X(25).
    05  CUST-MIDDLE-NAME                        PIC X(25).
    05  CUST-LAST-NAME                          PIC X(25).
    05  CUST-ADDR-LINE-1                        PIC X(50).
    05  CUST-ADDR-LINE-2                        PIC X(50).
    05  CUST-ADDR-LINE-3                        PIC X(50).
    05  CUST-ADDR-STATE-CD                      PIC X(02).
    05  CUST-ADDR-COUNTRY-CD                    PIC X(03).
    05  CUST-ADDR-ZIP                           PIC X(10).
    05  CUST-PHONE-NUM-1                        PIC X(15).
    05  CUST-PHONE-NUM-2                        PIC X(15).
    05  CUST-SSN                                PIC 9(09).
    05  CUST-GOVT-ISSUED-ID                     PIC X(20).
    05  CUST-DOB-YYYY-MM-DD                     PIC X(10).
    05  CUST-EFT-ACCOUNT-ID                     PIC X(10).
    05  CUST-PRI-CARD-HOLDER-IND                PIC X(01).
    05  CUST-FICO-CREDIT-SCORE                  PIC 9(03).
    05  FILLER                                  PIC X(168).
```

#### Field Descriptions:
- **CUST-ID**: Unique 9-digit customer identifier
- **Name Fields**: First, middle, last name (25 characters each)
- **Address Fields**: Three address lines plus state, country, ZIP
- **Phone Numbers**: Two phone number fields (formatted)
- **CUST-SSN**: Social Security Number (9 digits)
- **CUST-GOVT-ISSUED-ID**: Government ID (driver's license, etc.)
- **CUST-DOB**: Date of birth in YYYY-MM-DD format
- **CUST-EFT-ACCOUNT-ID**: Electronic funds transfer account
- **CUST-PRI-CARD-HOLDER-IND**: Primary cardholder indicator
- **CUST-FICO-CREDIT-SCORE**: Credit score (300-850 range)

#### Business Rules:
- Customer ID must be unique
- SSN must be unique and valid format
- Date of birth must be valid date and indicate adult age
- FICO score must be in valid range (300-850)
- At least one address line must be populated
- Phone numbers should follow standard formatting

---

### 3. Credit Card Data (CARDDAT)

**VSAM Dataset**: `AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS`  
**Copybook**: `CVACT02Y.cpy`  
**Record Length**: 150 bytes  
**Key Field**: `CARD-NUM` (PIC X(16))  

#### Record Structure:
```cobol
01  CARD-RECORD.
    05  CARD-NUM                          PIC X(16).
    05  CARD-ACCT-ID                      PIC 9(11).
    05  CARD-CVV-CD                       PIC 9(03).
    05  CARD-EMBOSSED-NAME                PIC X(50).
    05  CARD-EXPIRAION-DATE               PIC X(10).
    05  CARD-ACTIVE-STATUS                PIC X(01).
    05  FILLER                            PIC X(59).
```

#### Field Descriptions:
- **CARD-NUM**: 16-digit credit card number (unique identifier)
- **CARD-ACCT-ID**: Associated account ID (foreign key to ACCTDAT)
- **CARD-CVV-CD**: 3-digit Card Verification Value
- **CARD-EMBOSSED-NAME**: Name printed on card (up to 50 characters)
- **CARD-EXPIRAION-DATE**: Expiration date (MM/YY format)
- **CARD-ACTIVE-STATUS**: 'Y' (Active) or 'N' (Inactive)

#### Business Rules:
- Card number must be unique and follow credit card number algorithms
- Card must be associated with valid account ID
- CVV must be 3 digits
- Expiration date must be future date
- Active status controls transaction authorization
- Multiple cards can be associated with single account

#### Alternate Index:
**AIX Dataset**: `AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX.PATH`  
**Alternate Key**: `CARD-ACCT-ID`  
**Purpose**: Allows retrieval of all cards for a specific account

---

## Cross-Reference Files

### 4. Card-to-Account Cross-Reference (CCXREF)

**VSAM Dataset**: `AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS`  
**Copybook**: `CVACT03Y.cpy`  
**Record Length**: 50 bytes  
**Key Field**: `XREF-CARD-NUM` (PIC X(16))  

#### Record Structure:
```cobol
01 CARD-XREF-RECORD.
    05  XREF-CARD-NUM                     PIC X(16).
    05  XREF-CUST-ID                      PIC 9(09).
    05  XREF-ACCT-ID                      PIC 9(11).
    05  FILLER                            PIC X(14).
```

#### Purpose:
- Provides fast lookup from card number to customer and account
- Essential for transaction validation and authorization
- Eliminates need for complex joins during transaction processing
- Supports both online and batch processing requirements

#### Alternate Index:
**AIX Dataset**: `AWS.M2.CARDDEMO.CARDXREF.VSAM.AIX.PATH`  
**Alternate Key**: `XREF-ACCT-ID`  
**Purpose**: Find all cards associated with an account

---

## Transaction Files

### 5. Transaction Master File (TRANSACT)

**VSAM Dataset**: `AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS`  
**Copybook**: `CVTRA05Y.cpy`  
**Record Length**: 350 bytes  
**Key Field**: `TRANS-ID` (PIC X(16))  

#### Record Structure (Inferred from DALYTRAN):
```cobol
01  TRANSACTION-RECORD.
    05  TRANS-ID                             PIC X(16).
    05  TRANS-TYPE-CD                        PIC X(02).
    05  TRANS-CAT-CD                         PIC 9(04).
    05  TRANS-SOURCE                         PIC X(10).
    05  TRANS-DESC                           PIC X(100).
    05  TRANS-AMT                            PIC S9(09)V99.
    05  TRANS-MERCHANT-ID                    PIC 9(09).
    05  TRANS-MERCHANT-NAME                  PIC X(50).
    05  TRANS-MERCHANT-CITY                  PIC X(50).
    05  TRANS-MERCHANT-ZIP                   PIC X(10).
    05  TRANS-CARD-NUM                       PIC X(16).
    05  TRANS-ORIG-TS                        PIC X(26).
    05  TRANS-PROC-TS                        PIC X(26).
    05  FILLER                               PIC X(20).
```

#### Field Descriptions:
- **TRANS-ID**: Unique 16-character transaction identifier
- **TRANS-TYPE-CD**: Transaction type (purchase, payment, fee, etc.)
- **TRANS-CAT-CD**: Transaction category code (4 digits)
- **TRANS-SOURCE**: Source system or channel
- **TRANS-DESC**: Transaction description (100 characters)
- **TRANS-AMT**: Transaction amount (signed, 2 decimal places)
- **Merchant Fields**: ID, name, city, ZIP code
- **TRANS-CARD-NUM**: Associated credit card number
- **TRANS-ORIG-TS**: Original transaction timestamp (26 characters)
- **TRANS-PROC-TS**: Processing timestamp (26 characters)

#### Business Rules:
- Transaction ID must be unique across all transactions
- Transaction amount can be positive (charges) or negative (credits/payments)
- Card number must exist in CCXREF for validation
- Timestamps follow DB2 format: YYYY-MM-DD-HH.MM.SS.NNNNNN
- Transaction type and category must be valid codes
- Merchant information required for purchase transactions

#### Alternate Index:
**Purpose**: Support transaction queries by card number, date range, and amount
**Access Patterns**: Used for statement generation and transaction history

---

### 6. Daily Transaction File (DALYTRAN)

**Sequential Dataset**: `AWS.M2.CARDDEMO.DALYTRAN.PS`  
**Copybook**: `CVTRA06Y.cpy`  
**Record Length**: 350 bytes  
**Purpose**: Input file for batch transaction posting

#### Record Structure:
```cobol
01  DALYTRAN-RECORD.
    05  DALYTRAN-ID                             PIC X(16).
    05  DALYTRAN-TYPE-CD                        PIC X(02).
    05  DALYTRAN-CAT-CD                         PIC 9(04).
    05  DALYTRAN-SOURCE                         PIC X(10).
    05  DALYTRAN-DESC                           PIC X(100).
    05  DALYTRAN-AMT                            PIC S9(09)V99.
    05  DALYTRAN-MERCHANT-ID                    PIC 9(09).
    05  DALYTRAN-MERCHANT-NAME                  PIC X(50).
    05  DALYTRAN-MERCHANT-CITY                  PIC X(50).
    05  DALYTRAN-MERCHANT-ZIP                   PIC X(10).
    05  DALYTRAN-CARD-NUM                       PIC X(16).
    05  DALYTRAN-ORIG-TS                        PIC X(26).
    05  DALYTRAN-PROC-TS                        PIC X(26).
    05  FILLER                                  PIC X(20).
```

#### Processing Flow:
1. **Input**: Daily transactions from external systems
2. **Validation**: Card number, account status, amount limits
3. **Posting**: Valid transactions moved to TRANSACT file
4. **Rejection**: Invalid transactions written to DALYREJS file

---

## Security and Reference Files

### 7. User Security File (USRSEC)

**VSAM Dataset**: `AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS`  
**Copybook**: `CSUSR01Y.cpy`  
**Record Length**: 80 bytes  
**Key Field**: User ID (PIC X(08))  

#### Record Structure:
```cobol
01  SEC-USER-DATA.
    05  SEC-USR-ID                        PIC X(08).
    05  SEC-USR-PWD                       PIC X(08).
    05  SEC-USR-TYPE                      PIC X(01).
        88  SEC-USR-TYPE-ADMIN            VALUE 'A'.
        88  SEC-USR-TYPE-USER             VALUE 'U'.
    05  FILLER                            PIC X(63).
```

#### Security Features:
- User ID and password authentication
- Role-based access control (Admin vs User)
- Integration with CICS security
- Password validation and management

---

### 8. Supporting Reference Files

#### Transaction Category Balance (TCATBALF)
**VSAM Dataset**: `AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS`  
**Copybook**: `CVTRA01Y.cpy`  
**Purpose**: Track balances by transaction category for interest calculation

#### Disclosure Group (DISCGRP)
**VSAM Dataset**: `AWS.M2.CARDDEMO.DISCGRP.VSAM.KSDS`  
**Copybook**: `CVTRA02Y.cpy`  
**Purpose**: Interest rates and fees by account group and transaction type

#### Transaction Types (TRANTYPE)
**Sequential Dataset**: `AWS.M2.CARDDEMO.TRANTYPE.PS`  
**Copybook**: `CVTRA03Y.cpy`  
**Purpose**: Valid transaction type codes and descriptions

#### Transaction Categories (TRANCATG)
**Sequential Dataset**: `AWS.M2.CARDDEMO.TRANCATG.PS`  
**Copybook**: `CVTRA04Y.cpy`  
**Purpose**: Transaction category definitions and groupings

---

## Data Relationships and Dependencies

### Primary Relationships

```
CUSTOMER (CUST-ID) ←→ CARD-XREF (XREF-CUST-ID)
    ↓                        ↓
ACCOUNT (ACCT-ID)  ←→ CARD-XREF (XREF-ACCT-ID)
    ↓                        ↓
TRANSACTION (TRANS-CARD-NUM) ←→ CARD (CARD-NUM)
```

### Referential Integrity Rules

1. **Customer-Account Relationship**:
   - Each account must have associated customer
   - Customer can have multiple accounts
   - Account deletion requires customer validation

2. **Account-Card Relationship**:
   - Each card must be linked to valid account
   - Account can have multiple cards
   - Card status depends on account status

3. **Card-Transaction Relationship**:
   - Each transaction must reference valid card
   - Card must be active for new transactions
   - Transaction history preserved even if card deactivated

4. **Cross-Reference Integrity**:
   - CCXREF must maintain consistency with CARDDAT and ACCTDAT
   - Updates to master files require CCXREF synchronization
   - Orphaned cross-reference records indicate data integrity issues

### Data Access Patterns

#### Online Processing:
- **Authentication**: Direct read of USRSEC by User ID
- **Account Lookup**: Direct read of ACCTDAT by Account ID
- **Card Validation**: CCXREF lookup by Card Number, then CARDDAT read
- **Transaction Entry**: Sequential write to TRANSACT with unique key generation

#### Batch Processing:
- **Sequential Processing**: DALYTRAN → validation → TRANSACT posting
- **Account Updates**: Read-modify-write cycle for balance updates
- **Interest Calculation**: Sequential read of TCATBALF with account grouping
- **Statement Generation**: Multi-file join processing (CUSTDAT + ACCTDAT + TRANSACT)

---

## CICS File Definitions

From `CARDDEMO.CSD`, the following CICS file definitions control online access:

### File Control Table Entries:

```
DEFINE FILE(ACCTDAT) GROUP(CARDDEMO)
       DSNAME(AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS)
       STRINGS(1) STATUS(ENABLED) OPENTIME(FIRSTREF)
       ADD(YES) BROWSE(YES) DELETE(YES) READ(YES) UPDATE(YES)

DEFINE FILE(CARDDAT) GROUP(CARDDEMO)
       DSNAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS)
       
DEFINE FILE(CCXREF) GROUP(CARDDEMO)
       DSNAME(AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS)
       DESCRIPTION(CARD TO ACCOUNT XREF)

DEFINE FILE(CUSTDAT) GROUP(CARDDEMO)
       DSNAME(AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS)
       DESCRIPTION(CARDDEMO CUSTOMER DATA)

DEFINE FILE(TRANSACT) GROUP(CARDDEMO)
       DSNAME(AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS)

DEFINE FILE(USRSEC) GROUP(CARDDEMO)
       DSNAME(AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS)
```

### Access Control:
- **STRINGS(1)**: Single string access for file integrity
- **LSRPOOLNUM(1)**: Local Shared Resource pool assignment
- **READINTEG(UNCOMMITTED)**: Read consistency level
- **UPDATEMODEL(LOCKING)**: Record locking for updates

---

## Sample Data Migration Considerations

### EBCDIC to ASCII Conversion Requirements

The sample data files are stored in EBCDIC format and require careful conversion:

#### Character Data Conversion:
- **Customer Names**: EBCDIC → ASCII character mapping
- **Addresses**: Special character handling (accents, symbols)
- **Descriptions**: Transaction and merchant descriptions
- **Status Codes**: Single character fields ('Y'/'N', 'A'/'U')

#### Numeric Data Preservation:
- **Account IDs**: 11-digit numeric fields
- **Customer IDs**: 9-digit numeric fields
- **Card Numbers**: 16-digit numeric fields
- **Amounts**: Signed decimal fields with 2 decimal places

#### Date Format Conversion:
- **Current Format**: YYYY-MM-DD (10 characters)
- **Timestamp Format**: YYYY-MM-DD-HH.MM.SS.NNNNNN (26 characters)
- **Validation**: Leap year and date range checking

### Sample Data Files for Migration:

1. **USRSEC** (User Security):
   - Source: Inline data in DEFUSR01.jcl
   - Format: Fixed-length records, 80 bytes
   - Content: User ID, password, user type

2. **ACCTDATA** (Account Data):
   - Source: acctdata.txt
   - Format: Fixed-length records, 300 bytes
   - Content: Account master information

3. **CARDDATA** (Card Data):
   - Source: carddata.txt
   - Format: Fixed-length records, 150 bytes
   - Content: Credit card information

4. **CUSTDATA** (Customer Data):
   - Source: custdata.txt
   - Format: Fixed-length records, 500 bytes
   - Content: Customer master information

5. **CARDXREF** (Card Cross-Reference):
   - Source: cardxref.txt
   - Format: Fixed-length records, 50 bytes
   - Content: Card-to-account-to-customer relationships

6. **DALYTRAN** (Daily Transactions):
   - Source: dailytran.txt
   - Format: Fixed-length records, 350 bytes
   - Content: Sample transaction data for processing

7. **Reference Data Files**:
   - **DISCGRP**: discgrp.txt (Disclosure groups)
   - **TRANCATG**: trancatg.txt (Transaction categories)
   - **TRANTYPE**: trantype.txt (Transaction types)
   - **TCATBALF**: tcatbal.txt (Transaction category balances)

### Migration Strategy:

#### Phase 1: Data Extraction
- Export VSAM files to sequential format
- Convert EBCDIC to ASCII encoding
- Validate data integrity during conversion
- Preserve numeric precision and formatting

#### Phase 2: Schema Mapping
- Map COBOL data structures to modern database schemas
- Handle COMP and COMP-3 numeric formats
- Convert fixed-length records to normalized tables
- Establish foreign key relationships

#### Phase 3: Data Loading
- Load converted data into target database
- Validate referential integrity
- Create indexes for performance
- Establish data validation rules

#### Phase 4: Verification
- Compare record counts and totals
- Validate business rule compliance
- Test data access patterns
- Verify transaction processing logic

---

## Modernization Data Architecture Recommendations

### Target Database Design:

#### Relational Database Schema:
```sql
-- Customer table
CREATE TABLE customers (
    customer_id BIGINT PRIMARY KEY,
    first_name VARCHAR(25),
    middle_name VARCHAR(25),
    last_name VARCHAR(25),
    address_line1 VARCHAR(50),
    address_line2 VARCHAR(50),
    address_line3 VARCHAR(50),
    state_code CHAR(2),
    country_code CHAR(3),
    zip_code VARCHAR(10),
    phone_primary VARCHAR(15),
    phone_secondary VARCHAR(15),
    ssn CHAR(9),
    government_id VARCHAR(20),
    date_of_birth DATE,
    eft_account_id VARCHAR(10),
    primary_cardholder_flag CHAR(1),
    fico_score SMALLINT,
    created_date TIMESTAMP,
    updated_date TIMESTAMP
);

-- Account table
CREATE TABLE accounts (
    account_id BIGINT PRIMARY KEY,
    customer_id BIGINT REFERENCES customers(customer_id),
    active_status CHAR(1),
    current_balance DECIMAL(12,2),
    credit_limit DECIMAL(12,2),
    cash_credit_limit DECIMAL(12,2),
    open_date DATE,
    expiration_date DATE,
    reissue_date DATE,
    current_cycle_credit DECIMAL(12,2),
    current_cycle_debit DECIMAL(12,2),
    address_zip VARCHAR(10),
    group_id VARCHAR(10),
    created_date TIMESTAMP,
    updated_date TIMESTAMP
);

-- Credit card table
CREATE TABLE credit_cards (
    card_number VARCHAR(16) PRIMARY KEY,
    account_id BIGINT REFERENCES accounts(account_id),
    cvv_code CHAR(3),
    embossed_name VARCHAR(50),
    expiration_date DATE,
    active_status CHAR(1),
    created_date TIMESTAMP,
    updated_date TIMESTAMP
);

-- Transaction table
CREATE TABLE transactions (
    transaction_id VARCHAR(16) PRIMARY KEY,
    card_number VARCHAR(16) REFERENCES credit_cards(card_number),
    transaction_type CHAR(2),
    category_code CHAR(4),
    source_system VARCHAR(10),
    description VARCHAR(100),
    amount DECIMAL(11,2),
    merchant_id BIGINT,
    merchant_name VARCHAR(50),
    merchant_city VARCHAR(50),
    merchant_zip VARCHAR(10),
    original_timestamp TIMESTAMP,
    processed_timestamp TIMESTAMP,
    created_date TIMESTAMP
);
```

### Data Migration Benefits:
1. **Improved Performance**: Modern indexing and query optimization
2. **Scalability**: Horizontal scaling capabilities
3. **Integration**: Standard SQL/API access patterns
4. **Backup/Recovery**: Modern disaster recovery options
5. **Analytics**: Business intelligence and reporting capabilities
6. **Compliance**: Enhanced audit trails and data governance

### Migration Challenges:
1. **Data Volume**: Large historical transaction datasets
2. **Downtime**: Minimizing business disruption during migration
3. **Validation**: Ensuring data accuracy and completeness
4. **Performance**: Maintaining response time requirements
5. **Integration**: Coordinating with dependent systems
6. **Rollback**: Planning for migration failure scenarios

This data architecture documentation provides the foundation for understanding the current VSAM-based data model and planning its migration to modern database technologies while preserving all business logic and data relationships.
