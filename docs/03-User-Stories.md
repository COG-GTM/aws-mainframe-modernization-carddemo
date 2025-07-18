# CardDemo User Stories

## Overview

This document provides comprehensive user stories for both Regular Users and Admin Users of the CardDemo application. Each story includes acceptance criteria that define the expected behavior and business rules that must be preserved during modernization.

---

## Regular User Stories

### Authentication and Session Management

#### Story 1: User Login
**As a** Regular User  
**I want to** log into the CardDemo system securely  
**So that** I can access my account and card information  

**Acceptance Criteria:**
- Given I am on the login screen (CC00)
- When I enter my valid User ID (e.g., USER0001) and Password (e.g., PASSWORD)
- Then I should be authenticated against the USRSEC file
- And I should be redirected to the Main Menu (COMEN01C)
- And my user context should be established in the COMMAREA
- And my user type should be set to 'U' (User)

**Error Scenarios:**
- Given I enter an invalid User ID
- When I submit the login form
- Then I should see "User not found. Try again..."
- And I should remain on the login screen

- Given I enter a valid User ID but wrong password
- When I submit the login form  
- Then I should see "Wrong Password. Try again..."
- And the cursor should be positioned on the password field

#### Story 2: Session Navigation
**As a** Regular User  
**I want to** navigate between different functions using PF keys  
**So that** I can efficiently move through the application  

**Acceptance Criteria:**
- Given I am on any screen in the application
- When I press PF3
- Then I should return to the previous screen or main menu
- And the system should maintain my navigation context
- And any unsaved changes should be handled appropriately

---

### Account Management

#### Story 3: View Account Details
**As a** Regular User  
**I want to** view my account information  
**So that** I can see my current balance, credit limit, and account status  

**Acceptance Criteria:**
- Given I am logged in as a regular user
- When I select Account View (CAVW) from the menu
- And I enter a valid 11-digit account number
- Then I should see my account details including:
  - Account ID
  - Current Balance (formatted as currency)
  - Credit Limit
  - Cash Credit Limit
  - Account Status (Active/Inactive)
  - Open Date, Expiration Date, Reissue Date
  - Current Cycle Credit and Debit amounts
  - Associated customer information

**Data Validation:**
- Given I enter an invalid account number format
- When I submit the request
- Then I should see "Account number must be a non zero 11 digit number"

- Given I enter an account number that doesn't exist
- When I submit the request
- Then I should see "Did not find this account in account master file"

#### Story 4: Update Account Information
**As a** Regular User  
**I want to** update certain account information  
**So that** I can maintain accurate account details  

**Acceptance Criteria:**
- Given I am viewing my account details
- When I select Account Update (CAUP)
- Then I should see an editable form with current account information
- And I should be able to modify:
  - Account Status (Y/N)
  - Credit Limit (numeric, positive value)
  - Cash Credit Limit (must not exceed credit limit)
  - Address ZIP code
- When I save valid changes
- Then the account record should be updated in ACCTDAT
- And I should see a confirmation message
- And the changes should be immediately reflected

**Business Rules:**
- Cash Credit Limit cannot exceed Credit Limit
- Account Status must be 'Y' or 'N'
- Credit Limit must be a positive numeric value
- All date fields must be in CCYYMMDD format
- ZIP code must be valid format

---

### Credit Card Management

#### Story 5: List My Credit Cards
**As a** Regular User  
**I want to** see a list of all credit cards associated with my account  
**So that** I can manage my cards and select specific ones for detailed operations  

**Acceptance Criteria:**
- Given I am logged in as a regular user
- When I select Credit Card List (CCLI) from the menu
- Then I should see a paginated list showing:
  - Account Number (11 digits)
  - Card Number (16 digits, may be masked for security)
  - Card Status (Active/Inactive)
- And I should see up to 7 cards per screen
- And I should be able to navigate with PF7 (Page Up) and PF8 (Page Down)
- And I should only see cards associated with my account context

**Selection Options:**
- Given I am viewing the card list
- When I enter 'S' next to a card
- Then I should be able to view detailed card information
- When I enter 'U' next to a card
- Then I should be able to update that card's information
- And I should only be able to select one card at a time

#### Story 6: View Credit Card Details
**As a** Regular User  
**I want to** view detailed information about a specific credit card  
**So that** I can see card status, expiration date, and associated account information  

**Acceptance Criteria:**
- Given I have selected a card from the card list
- When I choose to view card details (CCDL)
- Then I should see comprehensive card information:
  - Card Number
  - CVV Code (masked for security)
  - Expiration Date
  - Card Status
  - Embossed Name
  - Associated Account Details
  - Customer Information
- And all information should be read-only in view mode
- And I should be able to navigate back to the card list

#### Story 7: Update Credit Card Information
**As a** Regular User  
**I want to** update my credit card information  
**So that** I can maintain accurate card details and status  

**Acceptance Criteria:**
- Given I have selected a card for update
- When I access the card update screen (CCUP)
- Then I should see an editable form with current card information
- And I should be able to modify:
  - Card Status (Active/Inactive)
  - Expiration Date
  - Embossed Name
- When I save valid changes
- Then the card record should be updated in CARDDAT
- And I should see a confirmation message

**Validation Rules:**
- Card Status must be 'Y' (Active) or 'N' (Inactive)
- Expiration Date must be in MM/YY format and in the future
- Embossed Name must be alphanumeric, maximum 50 characters
- CVV Code must be 3 digits (if editable)

---

### Transaction Management

#### Story 8: View Transaction History
**As a** Regular User  
**I want to** view my transaction history  
**So that** I can monitor my spending and account activity  

**Acceptance Criteria:**
- Given I am logged in as a regular user
- When I select Transaction List (CT00) from the menu
- Then I should see a list of my recent transactions including:
  - Transaction ID
  - Transaction Date and Time
  - Description
  - Amount (formatted as currency with sign)
  - Merchant Name
  - Transaction Type
- And transactions should be sorted by date (most recent first)
- And I should be able to page through multiple screens of transactions
- And I should only see transactions for my associated accounts

**Filtering Options:**
- Given I am viewing transaction history
- When I specify filter criteria (date range, card number, amount range)
- Then I should see only transactions matching those criteria
- And the filter should persist during pagination

#### Story 9: View Transaction Details
**As a** Regular User  
**I want to** view detailed information about a specific transaction  
**So that** I can see complete transaction details including merchant information  

**Acceptance Criteria:**
- Given I am viewing the transaction list
- When I select a specific transaction for detail view (CT01)
- Then I should see comprehensive transaction information:
  - Transaction ID and timestamps
  - Complete merchant information (name, city, ZIP)
  - Transaction amount and type
  - Associated card and account information
  - Transaction category and description
  - Processing status and dates
- And all information should be read-only
- And I should be able to navigate back to the transaction list

#### Story 10: Add New Transaction
**As a** Regular User  
**I want to** add a new transaction  
**So that** I can record purchases or payments  

**Acceptance Criteria:**
- Given I am logged in as a regular user
- When I select Add Transaction (CT02) from the menu
- Then I should see a transaction entry form
- And I should be able to enter:
  - Card Number (validated against my cards)
  - Transaction Amount (positive for purchases, negative for credits)
  - Merchant Name and Location
  - Transaction Description
  - Transaction Category
- When I submit a valid transaction
- Then the transaction should be added to TRANSACT file
- And my account balance should be updated immediately
- And I should receive a confirmation with transaction ID

**Validation Rules:**
- Card Number must exist in CCXREF and be associated with my account
- Card must be active status
- Transaction Amount must be numeric with up to 2 decimal places
- Account must have sufficient credit limit for purchases
- Merchant Name is required for all transactions
- Transaction Description cannot be empty

---

### Bill Payment

#### Story 11: Make Bill Payment
**As a** Regular User  
**I want to** make a payment toward my account balance  
**So that** I can reduce my outstanding balance  

**Acceptance Criteria:**
- Given I am logged in as a regular user
- When I select Bill Payment (CB00) from the menu
- Then I should see a payment form
- And I should be able to enter:
  - Payment Amount (positive numeric value)
  - Payment Date (current or future date)
  - Payment Reference/Description
- When I submit a valid payment
- Then a payment transaction should be created
- And my account balance should be reduced by the payment amount
- And I should receive a payment confirmation number

**Business Rules:**
- Payment Amount must be positive and numeric
- Payment Date cannot be in the past
- Payment cannot exceed current outstanding balance + pending charges
- Payment should be processed immediately for current date
- Future-dated payments should be scheduled appropriately

---

### Reports and Statements

#### Story 12: View Transaction Reports
**As a** Regular User  
**I want to** generate transaction reports  
**So that** I can analyze my spending patterns  

**Acceptance Criteria:**
- Given I am logged in as a regular user
- When I select Transaction Reports (CR00) from the menu
- Then I should be able to specify report criteria:
  - Date range
  - Transaction types
  - Card selection
  - Amount ranges
- When I generate the report
- Then I should see a formatted report showing:
  - Transaction summary by category
  - Total amounts by type (purchases, payments, fees)
  - Monthly spending trends
  - Merchant analysis
- And I should be able to print or save the report

---

## Admin User Stories

### User Management

#### Story 13: Admin Login and Access
**As an** Admin User  
**I want to** log into the system with administrative privileges  
**So that** I can manage users and perform administrative functions  

**Acceptance Criteria:**
- Given I am on the login screen (CC00)
- When I enter my admin credentials (e.g., ADMIN001/PASSWORD)
- Then I should be authenticated against the USRSEC file
- And my user type should be set to 'A' (Admin)
- And I should be redirected to the Admin Menu (COADM01C)
- And I should have access to administrative functions

#### Story 14: List All Users
**As an** Admin User  
**I want to** view a list of all system users  
**So that** I can manage user accounts and monitor system access  

**Acceptance Criteria:**
- Given I am logged in as an admin user
- When I select List Users (CU00) from the admin menu
- Then I should see a list of all users in the system including:
  - User ID
  - User Type (Admin/User)
  - Last Login Date (if available)
  - Account Status
- And I should be able to page through multiple screens of users
- And I should be able to select users for detailed operations

#### Story 15: Add New User
**As an** Admin User  
**I want to** add new users to the system  
**So that** I can grant access to authorized personnel  

**Acceptance Criteria:**
- Given I am logged in as an admin user
- When I select Add User (CU01) from the admin menu
- Then I should see a user creation form
- And I should be able to enter:
  - User ID (8 characters, alphanumeric)
  - Password (8 characters minimum)
  - User Type (Admin or User)
  - Additional user attributes
- When I submit valid user information
- Then the new user should be added to USRSEC file
- And I should receive confirmation of successful creation
- And the new user should be able to log in immediately

**Validation Rules:**
- User ID must be unique in the system
- User ID must be 8 characters or less, alphanumeric
- Password must meet security requirements
- User Type must be 'A' (Admin) or 'U' (User)
- All required fields must be completed

#### Story 16: Update User Information
**As an** Admin User  
**I want to** update existing user information  
**So that** I can maintain accurate user records and modify access levels  

**Acceptance Criteria:**
- Given I am viewing the user list
- When I select a user for update (CU02)
- Then I should see an editable form with current user information
- And I should be able to modify:
  - Password
  - User Type
  - Account Status (Active/Inactive)
- When I save valid changes
- Then the user record should be updated in USRSEC file
- And I should see a confirmation message
- And the changes should take effect immediately

#### Story 17: Delete User
**As an** Admin User  
**I want to** remove users from the system  
**So that** I can revoke access for users who no longer need system access  

**Acceptance Criteria:**
- Given I am viewing the user list
- When I select a user for deletion (CU03)
- Then I should see a confirmation prompt with user details
- And I should be warned about the permanent nature of deletion
- When I confirm the deletion
- Then the user record should be removed from USRSEC file
- And the user should no longer be able to log in
- And I should receive confirmation of successful deletion

**Safety Rules:**
- System should prevent deletion of the last admin user
- Confirmation should be required before deletion
- User should be logged out immediately if currently active
- Audit trail should record the deletion action

---

### System Administration

#### Story 18: View All Account Information
**As an** Admin User  
**I want to** view any account in the system  
**So that** I can provide customer support and investigate issues  

**Acceptance Criteria:**
- Given I am logged in as an admin user
- When I access account functions
- Then I should be able to view any account without restrictions
- And I should see all account details including:
  - Complete account information
  - Associated customer data
  - All linked credit cards
  - Complete transaction history
  - Account status and limits
- And I should have read-only access to sensitive information

#### Story 19: View All Credit Cards
**As an** Admin User  
**I want to** view all credit cards in the system  
**So that** I can manage card inventory and investigate card-related issues  

**Acceptance Criteria:**
- Given I am logged in as an admin user
- When I access the credit card list (CCLI)
- Then I should see all cards in the system regardless of account association
- And I should be able to filter and search cards by:
  - Account number
  - Card number
  - Customer name
  - Card status
- And I should be able to view and update any card information

#### Story 20: Monitor System Activity
**As an** Admin User  
**I want to** monitor system usage and transaction activity  
**So that** I can ensure system performance and detect unusual activity  

**Acceptance Criteria:**
- Given I am logged in as an admin user
- When I access system monitoring functions
- Then I should be able to view:
  - Current active users
  - Recent transaction volumes
  - System performance metrics
  - Error logs and exceptions
  - File status and availability
- And I should be able to generate administrative reports

---

## Cross-Functional Stories

#### Story 21: System Error Handling
**As a** User (Regular or Admin)  
**I want to** receive clear error messages when problems occur  
**So that** I can understand what went wrong and how to proceed  

**Acceptance Criteria:**
- Given any system error occurs (file access, validation, etc.)
- When the error is encountered
- Then I should see a clear, user-friendly error message
- And the message should indicate what action I can take
- And the system should remain stable and allow me to continue
- And technical details should be logged for administrator review

#### Story 22: Data Consistency
**As a** User (Regular or Admin)  
**I want to** see consistent data across all screens and functions  
**So that** I can trust the information displayed  

**Acceptance Criteria:**
- Given I view the same data from different screens
- When the data is displayed
- Then all screens should show identical information
- And updates made in one area should be reflected everywhere
- And the system should maintain referential integrity
- And concurrent access should be handled properly

#### Story 23: Session Management
**As a** User (Regular or Admin)  
**I want to** maintain my session context as I navigate  
**So that** I don't lose my work or have to re-enter information  

**Acceptance Criteria:**
- Given I am working in the application
- When I navigate between screens
- Then my user context should be preserved
- And my navigation history should be maintained
- And I should be able to return to previous screens
- And any work in progress should be preserved appropriately

## Acceptance Testing Considerations

### Data Setup Requirements:
- Test users in USRSEC file (both regular and admin)
- Sample accounts in ACCTDAT with various statuses
- Test credit cards in CARDDAT linked to test accounts
- Sample transactions in TRANSACT for testing history
- Cross-reference data in CCXREF linking cards to accounts

### Security Testing:
- Verify user authentication works correctly
- Confirm authorization rules prevent unauthorized access
- Test session timeout and security
- Validate data access restrictions by user type

### Integration Testing:
- Verify VSAM file operations work correctly
- Test CICS transaction flow and program linkage
- Confirm COMMAREA data passing between programs
- Validate batch job integration with online data

### Performance Testing:
- Test response times for interactive transactions
- Verify system handles concurrent users
- Test large data volume scenarios
- Validate batch processing performance

These user stories provide the foundation for testing the modernized application and ensuring that all business functionality is preserved during the transformation process.
