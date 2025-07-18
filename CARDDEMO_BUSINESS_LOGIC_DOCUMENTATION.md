# CardDemo Application Business Logic and Workflows Documentation

## Table of Contents

1. [Executive Summary](#1-executive-summary)
2. [User Roles and Permissions](#2-user-roles-and-permissions)
3. [User Workflows](#3-user-workflows)
4. [Batch Processing Workflows](#4-batch-processing-workflows)
5. [Data Validation Rules](#5-data-validation-rules)
6. [User Stories](#6-user-stories)
7. [Data Model](#7-data-model)
8. [Sample Data Structures](#8-sample-data-structures)
9. [Integration Points](#9-integration-points)
10. [Appendices](#10-appendices)

---

## 1. Executive Summary

The CardDemo application is a comprehensive credit card management system designed for mainframe environments using COBOL, CICS, and VSAM technologies. This documentation serves as the foundation for migrating the application to a modern technology stack.

### Application Overview

CardDemo provides complete credit card lifecycle management including:
- **Customer and Account Management**: Account creation, updates, and viewing
- **Credit Card Operations**: Card issuance, updates, status management
- **Transaction Processing**: Real-time transaction entry and historical tracking
- **Bill Payment Processing**: Online payment capabilities
- **Statement Generation**: Automated monthly statement creation
- **User Administration**: Role-based access control
- **Batch Processing**: Daily transaction posting and interest calculations

### Key Business Capabilities

1. **Dual User Model**: Regular users and administrative users with distinct permissions
2. **Real-time Processing**: Online transaction processing through CICS
3. **Batch Operations**: End-of-day processing for validation and reporting
4. **Data Integrity**: Comprehensive validation rules and cross-reference checks
5. **Audit Trail**: Complete transaction history and timestamp tracking

---

## 2. User Roles and Permissions

### User Types

#### Regular User (Type: 'U')
**Functions:**
- Account management (view/update personal accounts)
- Credit card management (view/update card details)
- Transaction management (view history, create transactions)
- Bill payment processing

**Entry Point:** Main Menu (COMEN01C)

#### Admin User (Type: 'A')
**Functions:**
- User administration (list, add, update, delete users)
- System-wide access to all accounts and transactions
- User security management

**Entry Point:** Admin Menu (COADM01C)

### Permission Matrix

| Function | Transaction ID | Program | Regular User | Admin User |
|----------|---------------|---------|--------------|------------|
| Signon Screen | CC00 | COSGN00C | ✓ | ✓ |
| Main Menu | CM00 | COMEN01C | ✓ | ✗ |
| Account View | CAVW | COACTVWC | ✓ | ✗ |
| Account Update | CAUP | COACTUPC | ✓ | ✗ |
| Credit Card List | CCLI | COCRDLIC | ✓ | ✗ |
| Credit Card Update | CCUP | COCRDUPC | ✓ | ✗ |
| Transaction List | CT00 | COTRN00C | ✓ | ✗ |
| Bill Payment | CB00 | COBIL00C | ✓ | ✗ |
| Admin Menu | CA00 | COADM01C | ✗ | ✓ |
| List Users | CU00 | COUSR00C | ✗ | ✓ |
| Add User | CU01 | COUSR01C | ✗ | ✓ |
| Update User | CU02 | COUSR02C | ✗ | ✓ |
| Delete User | CU03 | COUSR03C | ✗ | ✓ |

---

## 3. User Workflows

### 3.1 Regular User Workflows

#### Account Management Flow (COACTVWC, COACTUPC)
1. **Account Viewing**: User provides 11-digit account number → System validates and displays account details
2. **Account Updates**: User modifies account information → System validates changes → Updates account record

#### Credit Card Management Flow (COCRDLIC, COCRDUPC)
1. **Card Listing**: Display all cards for user's accounts with pagination
2. **Card Updates**: Modify embossed name, expiration date, and status with validation

#### Transaction Management Flow (COTRN00C, COTRN01C, COTRN02C)
1. **Transaction Listing**: Display transaction history with search and pagination
2. **Transaction Details**: Show complete transaction information
3. **Transaction Creation**: Enter new transactions with validation

#### Bill Payment Flow (COBIL00C)
1. **Account Selection**: User provides account ID
2. **Balance Verification**: Display current balance
3. **Payment Confirmation**: Require explicit confirmation
4. **Payment Processing**: Create payment transaction and update balance

### 3.2 Admin User Workflows

#### User Administration Flow (COUSR00C-COUSR03C)
1. **User Listing**: Display all system users with pagination
2. **User Creation**: Add new users with role assignment
3. **User Updates**: Modify user information and permissions
4. **User Deletion**: Remove users with confirmation

---

## 4. Batch Processing Workflows

### Processing Sequence
1. **POSTTRAN** - Transaction posting (CBTRN02C)
2. **INTCALC** - Interest calculation (CBACT04C)
3. **CREASTMT** - Statement generation (CBSTM03A)

### Transaction Validation (CBTRN01C)
- Read daily transaction file
- Validate card numbers against XREFFILE
- Verify account existence and status
- Log errors for invalid transactions

### Interest Calculation (CBACT04C)
- Process transaction category balances
- Calculate interest based on account group rates
- Update account balances with interest charges
- Generate interest transaction records

### Statement Generation (CBSTM03A)
- Generate customer statements in text and HTML formats
- Include account details, transaction summaries, and balances
- Process all active accounts with transaction activity

---

## 5. Data Validation Rules

### Account Validation
- **Account ID**: 11-digit numeric, non-zero, must exist in ACCTDAT
- **Account Status**: 'Y' (active) or 'N' (inactive)
- **Credit Limits**: Positive numeric values with 2 decimal places
- **Dates**: YYYY-MM-DD format with validity checks

### Credit Card Validation
- **Card Number**: 16-digit numeric, must exist in CARDDAT
- **CVV Code**: 3-digit numeric (001-999)
- **Embossed Name**: Alphabetic characters and spaces only
- **Expiration Date**: Month 1-12, year 1950-2099, future date
- **Card Status**: 'Y' (active) or 'N' (inactive)

### Transaction Validation
- **Transaction ID**: 16-character unique identifier
- **Amount**: Positive numeric with 2 decimal places
- **Transaction Type**: Must exist in TRANTYPE file
- **Card Association**: Card must be active and valid

### User Management Validation
- **User ID**: 8-character alphanumeric, unique
- **Password**: 8-character field, required for new users
- **User Type**: 'A' (admin) or 'U' (regular user)
- **Names**: Alphabetic characters and spaces only

---

## 6. User Stories

### Authentication and Access
- **As a** system user, **I want to** log in with my credentials, **so that** I can access authorized functions
- **As a** system administrator, **I want to** have role-based permissions, **so that** users access appropriate functions

### Account Management
- **As a** regular user, **I want to** view my account details, **so that** I can monitor my balance and credit limits
- **As a** regular user, **I want to** update my account information, **so that** I can keep my details current

### Credit Card Management
- **As a** regular user, **I want to** view all my credit cards, **so that** I can manage multiple cards
- **As a** regular user, **I want to** update card details, **so that** I can maintain accurate information

### Transaction Management
- **As a** regular user, **I want to** view transaction history, **so that** I can review my spending
- **As a** regular user, **I want to** create new transactions, **so that** I can record card activity

### Bill Payment
- **As a** regular user, **I want to** pay my account balance, **so that** I can manage my payments conveniently

### User Administration
- **As an** admin user, **I want to** manage user accounts, **so that** I can control system access
- **As an** admin user, **I want to** create new users, **so that** customers can access the system

---

## 7. Data Model

### Core Entities

#### Customer Entity (CUSTDAT)
- **CUST-ID** (PIC 9(09)) - Primary key
- **CUST-FNAME/LNAME** (PIC X(25)) - Customer names
- **CUST-ADDR-LINE-1/2/3** (PIC X(50)) - Address information
- **CUST-SSN** (PIC 9(09)) - Social Security Number
- **CUST-FICO-CREDIT-SCORE** (PIC 9(03)) - Credit score

#### Account Entity (ACCTDAT)
- **ACCT-ID** (PIC 9(11)) - Primary key
- **ACCT-CURR-BAL** (PIC S9(10)V99) - Current balance
- **ACCT-CREDIT-LIMIT** (PIC S9(10)V99) - Credit limit
- **ACCT-ACTIVE-STATUS** (PIC X(01)) - Account status
- **ACCT-GROUP-ID** (PIC X(10)) - Interest rate group

#### Card Entity (CARDDAT)
- **CARD-NUM** (PIC X(16)) - Primary key
- **CARD-ACCT-ID** (PIC 9(11)) - Foreign key to Account
- **CARD-CVV-CD** (PIC 9(03)) - Card verification value
- **CARD-EMBOSSED-NAME** (PIC X(50)) - Name on card
- **CARD-ACTIVE-STATUS** (PIC X(01)) - Card status

#### Transaction Entity (TRANSACT)
- **TRAN-ID** (PIC X(16)) - Primary key
- **TRAN-AMT** (PIC S9(09)V99) - Transaction amount
- **TRAN-CARD-NUM** (PIC X(16)) - Foreign key to Card
- **TRAN-TYPE-CD** (PIC X(02)) - Transaction type
- **TRAN-MERCHANT-NAME** (PIC X(50)) - Merchant information

#### Cross-Reference Entity (XREFFILE)
- **XREF-CARD-NUM** (PIC X(16)) - Primary key
- **XREF-CUST-ID** (PIC 9(09)) - Foreign key to Customer
- **XREF-ACCT-ID** (PIC 9(11)) - Foreign key to Account

#### User Security Entity (USRSEC)
- **SEC-USR-ID** (PIC X(08)) - Primary key
- **SEC-USR-TYPE** (PIC X(01)) - User type ('A'/'U')
- **SEC-USR-PWD** (PIC X(08)) - Password

### Entity Relationships
- Customer → Account (1:N)
- Account → Card (1:N)
- Card → Transaction (1:N)
- Cross-Reference links Customer, Account, and Card entities

---

## 8. Sample Data Structures

### Key Data Files
- **ACCTFILE**: Account master data with balances and limits
- **CARDFILE**: Credit card information and status
- **CUSTFILE**: Customer demographic and contact information
- **XREFFILE**: Card-account-customer relationships
- **TRANSACT**: Transaction history and details
- **USRSEC**: User authentication and authorization
- **DISCGRP**: Interest rate and fee structures

### Migration Considerations
- EBCDIC to ASCII character conversion required
- COMP and COMP-3 numeric formats need conversion
- Date formats standardization (YYYY-MM-DD)
- Decimal precision handling for monetary amounts

---

## 9. Integration Points

### CICS Integration
- Online transaction processing through CICS
- Screen handling via Basic Mapping Support (BMS)
- Program-to-program communication using COMMAREA

### VSAM File Interfaces
- KSDS (Key Sequenced Data Sets) for master files
- Alternate indexes for cross-reference access
- Sequential processing for batch operations

### Batch Job Interfaces
- JCL job scheduling and execution
- File backup and recovery procedures
- Generation Data Groups (GDG) for historical data

### External System Touchpoints
- RACF security system integration
- System timestamp and date services
- Error logging and audit trail systems

---

## 10. Appendices

### Appendix A: Program Analysis Summary

**Online Programs (CICS):**
- COSGN00C: User authentication and session management
- COACTVWC: Account viewing with validation and display
- COACTUPC: Account updates with comprehensive field validation
- COCRDLIC: Credit card listing with pagination and selection
- COCRDUPC: Credit card updates with business rule enforcement
- COTRN00C: Transaction listing with search and navigation
- COTRN01C: Transaction detail display
- COTRN02C: Transaction creation and validation
- COBIL00C: Bill payment processing with confirmation
- COUSR00C-COUSR03C: Complete user administration suite

**Batch Programs:**
- CBTRN01C: Daily transaction validation and cross-reference checking
- CBTRN02C: Transaction posting and balance updates
- CBACT04C: Interest calculation with rate table lookup
- CBSTM03A: Multi-format statement generation

### Appendix B: Business Rule Matrix
- Complete validation rules for all data elements
- Cross-reference validation requirements
- Business constraint enforcement
- Error handling and user feedback specifications

### Appendix C: Migration Considerations
- Data conversion requirements from mainframe formats
- Business logic extraction for modern implementation
- User interface modernization requirements
- Security and audit trail preservation needs

---

*This documentation serves as the comprehensive specification for migrating the CardDemo application from its mainframe COBOL/CICS implementation to a modern technology stack while preserving all business logic and functionality.*
