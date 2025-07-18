# CardDemo Key Workflows Documentation

## Overview

This document details the core business processes and workflows within the CardDemo application, covering both online CICS transactions and batch processing operations. Each workflow represents critical business functionality that must be preserved during modernization efforts.

## Online Transaction Workflows

### 1. User Authentication Flow (COSGN00C)

**Transaction ID**: CC00  
**Program**: COSGN00C  
**BMS Map**: COSGN00  

#### Process Flow:
1. **Initial Screen Display**
   - User accesses CC00 transaction
   - System displays signon screen with fields for User ID and Password
   - Screen populated with current date, time, and system information

2. **Input Validation**
   - System validates User ID is not spaces or low-values
   - System validates Password is not spaces or low-values
   - Input converted to uppercase for consistency

3. **Security File Lookup**
   - System reads USRSEC VSAM file using User ID as key
   - File structure: 8-byte User ID, Password, User Type (A/U)
   - CICS READ operation with comprehensive error handling

4. **Authentication Processing**
   ```cobol
   IF SEC-USR-PWD = WS-USER-PWD
       MOVE WS-USER-ID   TO CDEMO-USER-ID
       MOVE SEC-USR-TYPE TO CDEMO-USER-TYPE
       IF CDEMO-USRTYP-ADMIN
           EXEC CICS XCTL PROGRAM ('COADM01C')
       ELSE
           EXEC CICS XCTL PROGRAM ('COMEN01C')
       END-IF
   ```

5. **Navigation Based on User Type**
   - **Admin Users**: Transfer control to COADM01C (Admin Menu)
   - **Regular Users**: Transfer control to COMEN01C (Main Menu)
   - **Failed Authentication**: Display error message and re-prompt

#### Error Handling:
- Invalid User ID: "User not found. Try again..."
- Wrong Password: "Wrong Password. Try again..."
- File Access Error: "Unable to verify the User..."

#### COMMAREA Usage:
- Populates CARDDEMO-COMMAREA with user context
- Sets FROM-TRANID and FROM-PROGRAM for navigation tracking
- Establishes user session context for subsequent transactions

---

### 2. Account Management Workflows

#### 2.1 Account View (COACTVWC)

**Transaction ID**: CAVW  
**Program**: COACTVWC  
**BMS Map**: COACTVW  

#### Process Flow:
1. **Input Collection**
   - User enters Account ID (11-digit numeric)
   - System validates account number format and range

2. **Account Lookup Process**
   - Read CCXREF file to validate account exists in cross-reference
   - Read ACCTDAT file to retrieve account master data
   - Read CUSTDAT file to get associated customer information

3. **Data Presentation**
   - Display account details: ID, status, balances, limits
   - Show customer information: name, address, contact details
   - Present data in formatted screen layout

4. **Navigation Options**
   - PF3: Return to calling program or main menu
   - Enter: Refresh display with new account ID

#### 2.2 Account Update (COACTUPC)

**Transaction ID**: CAUP  
**Program**: COACTUPC  
**BMS Map**: COACTUP  

#### Process Flow:
1. **Account Retrieval**
   - Similar lookup process as Account View
   - Load current account data into update screen

2. **Field Validation**
   - **Account Status**: Must be 'Y' (Active) or 'N' (Inactive)
   - **Credit Limit**: Numeric validation, range checking
   - **Cash Credit Limit**: Must not exceed credit limit
   - **Current Balance**: Signed numeric with decimal validation
   - **Dates**: CCYYMMDD format validation with leap year logic

3. **Data Integrity Checks**
   - Verify account exists before update
   - Check for concurrent updates using record locking
   - Validate business rules (e.g., cash limit ≤ credit limit)

4. **Update Processing**
   ```cobol
   REWRITE FD-ACCTFILE-REC FROM ACCOUNT-RECORD
   IF ACCTFILE-STATUS = '00'
       MOVE 'Account updated successfully' TO WS-MESSAGE
   ELSE
       PERFORM ERROR-HANDLING
   END-IF
   ```

5. **Confirmation and Navigation**
   - Display success/error message
   - Option to update another account or return to menu

---

### 3. Card Management Workflows

#### 3.1 Card List (COCRDLIC)

**Transaction ID**: CCLI  
**Program**: COCRDLIC  
**BMS Map**: COCRDLI  

#### Process Flow:
1. **Access Control**
   - **Admin Users**: Can view all cards in system
   - **Regular Users**: Limited to cards associated with their account context

2. **Data Retrieval**
   - Browse CARDDAT VSAM file sequentially
   - Apply filtering based on user permissions
   - Load up to 7 cards per screen for display

3. **Screen Population**
   ```cobol
   PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-MAX-SCREEN-LINES
       MOVE CARD-ACCT-ID TO WS-ROW-ACCTNO(I)
       MOVE CARD-NUM TO WS-ROW-CARD-NUM(I)
       MOVE CARD-ACTIVE-STATUS TO WS-ROW-CARD-STATUS(I)
   END-PERFORM
   ```

4. **User Interaction**
   - User can select cards with 'S' (View) or 'U' (Update)
   - PF7/PF8 for page up/down navigation
   - Input validation for selection codes

5. **Navigation Processing**
   - 'S' selection: XCTL to COCRDSLC (Card Detail View)
   - 'U' selection: XCTL to COCRDUPC (Card Update)
   - Pass selected card context in COMMAREA

#### 3.2 Card Detail View (COCRDSLC)

**Transaction ID**: CCDL  
**Program**: COCRDSLC  
**BMS Map**: COCRDSL  

#### Process Flow:
1. **Card Data Retrieval**
   - Read CARDDAT using card number as key
   - Read associated ACCTDAT for account information
   - Read CUSTDAT for customer details

2. **Comprehensive Display**
   - Card details: Number, CVV, expiration, status
   - Account information: Balance, limits, status
   - Customer data: Name, address, contact information

3. **Related Data Access**
   - Show recent transactions for the card
   - Display account activity summary
   - Present credit utilization information

#### 3.3 Card Update (COCRDUPC)

**Transaction ID**: CCUP  
**Program**: COCRDUPC  
**BMS Map**: COCRDUP  

#### Process Flow:
1. **Current Data Display**
   - Load existing card information
   - Present editable fields to user

2. **Field-Level Validation**
   - **Card Status**: 'Y' (Active) or 'N' (Inactive)
   - **Expiration Date**: Future date validation
   - **CVV Code**: 3-digit numeric validation
   - **Embossed Name**: Alpha-numeric with length limits

3. **Update Processing**
   - Validate all changes before committing
   - Update CARDDAT record with new values
   - Log changes for audit trail

---

### 4. Transaction Processing Workflows

#### 4.1 Transaction List (COTRN00C)

**Transaction ID**: CT00  
**Program**: COTRN00C  
**BMS Map**: COTRN00  

#### Process Flow:
1. **Filter Criteria**
   - Date range selection
   - Card number filtering
   - Transaction type filtering

2. **Data Retrieval**
   - Read TRANSACT VSAM file with alternate index
   - Apply user-specified filters
   - Sort by transaction date/time

3. **Paginated Display**
   - Show transaction summary information
   - Support forward/backward navigation
   - Highlight different transaction types

#### 4.2 Transaction Detail View (COTRN01C)

**Transaction ID**: CT01  
**Program**: COTRN01C  
**BMS Map**: COTRN01  

#### Process Flow:
1. **Detailed Transaction Display**
   - Complete transaction information
   - Merchant details and location
   - Processing timestamps and status

2. **Related Information**
   - Associated card and account details
   - Transaction category and type
   - Authorization and settlement data

#### 4.3 Transaction Entry (COTRN02C)

**Transaction ID**: CT02  
**Program**: COTRN02C  
**BMS Map**: COTRN02  

#### Process Flow:
1. **Transaction Data Entry**
   - Card number validation against CCXREF
   - Amount entry with decimal validation
   - Merchant information capture
   - Transaction type and category selection

2. **Real-Time Validation**
   - Card status verification
   - Account balance checking
   - Credit limit validation
   - Duplicate transaction detection

3. **Transaction Creation**
   ```cobol
   MOVE FUNCTION CURRENT-DATE TO TRAN-ORIG-TS
   MOVE WS-GENERATED-ID TO TRAN-ID
   WRITE TRANSACT-RECORD
   IF TRANFILE-STATUS = '00'
       MOVE 'Transaction added successfully' TO WS-MESSAGE
   END-IF
   ```

4. **Account Balance Update**
   - Read current account balance
   - Apply transaction amount
   - Update account record
   - Handle insufficient funds scenarios

---

### 5. Bill Payment Process (COBIL00C)

**Transaction ID**: CB00  
**Program**: COBIL00C  
**BMS Map**: COBIL00  

#### Process Flow:
1. **Payment Information Entry**
   - Account selection
   - Payment amount validation
   - Payment date specification
   - Reference information capture

2. **Balance Verification**
   - Check current account balance
   - Validate sufficient funds available
   - Consider pending transactions

3. **Payment Processing**
   - Create payment transaction record
   - Update account balance
   - Generate confirmation number
   - Schedule payment execution

4. **Confirmation and Receipt**
   - Display payment confirmation
   - Provide transaction reference
   - Update transaction history

---

## Batch Processing Workflows

### 1. Transaction Posting Process (CBTRN02C)

**Job Name**: POSTTRAN  
**Program**: CBTRN02C  

#### Process Flow:
1. **File Initialization**
   - Open DALYTRAN (daily transaction input)
   - Open TRANSACT (transaction master file)
   - Open XREFFILE (card cross-reference)
   - Open ACCTFILE (account master)
   - Open TCATBALF (transaction category balance)

2. **Transaction Processing Loop**
   ```cobol
   PERFORM UNTIL END-OF-FILE = 'Y'
       PERFORM 1000-DALYTRAN-GET-NEXT
       PERFORM 1500-VALIDATE-TRAN
       IF WS-VALIDATION-FAIL-REASON = 0
           PERFORM 2000-POST-TRANSACTION
       ELSE
           PERFORM 2500-WRITE-REJECT-REC
       END-IF
   END-PERFORM
   ```

3. **Transaction Validation**
   - **Card Validation**: Lookup in XREFFILE
   - **Account Validation**: Verify account exists and is active
   - **Amount Validation**: Check for reasonable transaction amounts
   - **Duplicate Detection**: Prevent duplicate transaction processing

4. **Transaction Posting**
   - Write validated transaction to TRANSACT file
   - Update account balances in ACCTFILE
   - Update category balances in TCATBALF
   - Generate transaction ID and timestamps

5. **Error Handling**
   - Write rejected transactions to DALYREJS file
   - Include rejection reason codes and descriptions
   - Maintain processing statistics and counts

### 2. Interest Calculation (CBACT04C)

**Job Name**: INTCALC  
**Program**: CBACT04C  

#### Process Flow:
1. **Account Processing**
   - Read TCATBALF file sequentially
   - Group transactions by account ID
   - Calculate interest for each account

2. **Interest Computation**
   ```cobol
   PERFORM 1300-COMPUTE-INTEREST
   COMPUTE WS-MONTHLY-INT = 
       TRANCAT-BAL-AMT * DIS-INT-RATE / 100 / 12
   ADD WS-MONTHLY-INT TO WS-TOTAL-INT
   ```

3. **Account Balance Updates**
   - Add calculated interest to account balance
   - Reset current cycle credit/debit amounts
   - Update account master record

4. **Interest Transaction Creation**
   - Generate interest charge transactions
   - Post to transaction master file
   - Maintain audit trail of interest calculations

### 3. Statement Generation (CBSTM03A)

**Job Name**: CREASTMT  
**Program**: CBSTM03A  

#### Process Flow:
1. **Control Block Processing**
   - Access mainframe control blocks (PSA, TCB, TIOT)
   - Display job and step information
   - Validate DD name allocations

2. **Data Collection**
   - Read customer information from CUSTDAT
   - Retrieve account details from ACCTDAT
   - Collect transactions from TRANSACT file
   - Build transaction summary arrays

3. **Statement Formatting**
   - **Text Format**: Fixed-width formatted statements
   - **HTML Format**: Web-ready statement presentation
   - Customer information header
   - Account summary section
   - Transaction detail listing

4. **Output Generation**
   ```cobol
   PERFORM 5000-CREATE-STATEMENT
   WRITE STMT-FILE-REC FROM ST-LINE0
   WRITE HTML-FILE-REC FROM HTML-L01
   ```

5. **Multi-Format Output**
   - Generate both text and HTML versions
   - Maintain consistent formatting
   - Include all required statement elements

---

## Batch Processing Sequence

The complete batch processing workflow follows this dependency chain:

1. **CLOSEFIL** - Close VSAM files in CICS
2. **ACCTFILE** - Refresh Account Master
3. **CARDFILE** - Refresh Card Master  
4. **XREFFILE** - Load Card-Account Cross Reference
5. **CUSTFILE** - Refresh Customer Master
6. **TRANBKP** - Backup Transaction Master
7. **DISCGRP** - Load Disclosure Group File
8. **TCATBALF** - Load Transaction Category Balance
9. **TRANTYPE** - Load Transaction Type File
10. **DUSRSECJ** - Setup User Security File
11. **POSTTRAN** - Core Transaction Processing (CBTRN02C)
12. **INTCALC** - Interest Calculation (CBACT04C)
13. **TRANBKP** - Backup Transaction Master (post-processing)
14. **COMBTRAN** - Combine Transaction Files
15. **CREASTMT** - Generate Statements (CBSTM03A)
16. **TRANIDX** - Define Alternate Index on Transaction File
17. **OPENFIL** - Open VSAM files for CICS

### Critical Dependencies:
- Master file refreshes must complete before transaction processing
- Cross-reference files must be loaded before validation
- Transaction posting must complete before interest calculation
- Interest calculation must complete before statement generation
- All processing must complete before files are reopened for CICS

## Modernization Workflow Considerations

### Preservation Requirements:
1. **Business Logic Integrity**: All validation rules and calculations must be preserved
2. **Data Consistency**: Transaction atomicity and file integrity must be maintained
3. **Error Handling**: Comprehensive error detection and recovery mechanisms
4. **Audit Trail**: Complete transaction logging and change tracking
5. **Security Context**: User authentication and authorization patterns

### Transformation Opportunities:
1. **API Exposure**: Convert CICS transactions to REST/GraphQL APIs
2. **Database Migration**: Replace VSAM with relational or NoSQL databases
3. **Microservices**: Decompose monolithic batch jobs into service-oriented architecture
4. **Real-time Processing**: Replace batch processing with event-driven architecture
5. **Modern UI**: Transform 3270 screens to web/mobile interfaces

This workflow documentation provides the foundation for understanding the current business processes and planning their modernization while preserving critical functionality.
