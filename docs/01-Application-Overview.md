# CardDemo Application Overview

## Introduction

CardDemo is a comprehensive mainframe credit card management application designed to demonstrate mainframe modernization use-cases including discovery, migration, modernization, performance testing, augmentation, service enablement, service extraction, and test creation. The application showcases traditional mainframe technologies (COBOL, CICS, VSAM, JCL) while serving as a reference implementation for organizations modernizing legacy systems.

## Application Purpose and Scope

**Primary Function**: Credit card account and transaction management system
**Target Environment**: IBM mainframe with CICS transaction server and VSAM data storage
**Modernization Focus**: Demonstrates various approaches to mainframe application transformation

### Key Capabilities

- **Account Management**: View and update customer account information, credit limits, and balances
- **Credit Card Management**: List, view, and update credit card details and status
- **Transaction Processing**: Real-time transaction entry, validation, and reporting
- **User Authentication**: Secure login with role-based access control
- **Statement Generation**: Automated customer statement creation in multiple formats (text and HTML)
- **Administrative Functions**: User management and system administration
- **Batch Processing**: Background transaction posting, interest calculation, and file maintenance

## User Types and Permissions

### Regular User (User Type: 'U')
**Default Credentials**: USER0001/PASSWORD

**Permitted Functions**:
- **Account Operations**:
  - View account details (Transaction: CAVW, Program: COACTVWC)
  - Update account information (Transaction: CAUP, Program: COACTUPC)
- **Credit Card Operations**:
  - List credit cards (Transaction: CCLI, Program: COCRDLIC)
  - View card details (Transaction: CCDL, Program: COCRDSLC)
  - Update card information (Transaction: CCUP, Program: COCRDUPC)
- **Transaction Operations**:
  - List transactions (Transaction: CT00, Program: COTRN00C)
  - View transaction details (Transaction: CT01, Program: COTRN01C)
  - Add new transactions (Transaction: CT02, Program: COTRN02C)
  - Generate transaction reports (Transaction: CR00, Program: CORPT00C)
- **Bill Payment**:
  - Process bill payments (Transaction: CB00, Program: COBIL00C)

**Access Restrictions**: Cannot perform administrative functions or user management

### Admin User (User Type: 'A')
**Default Credentials**: ADMIN001/PASSWORD

**Permitted Functions**:
- **User Management**:
  - List users (Transaction: CU00, Program: COUSR00C)
  - Add new users (Transaction: CU01, Program: COUSR01C)
  - Update user information (Transaction: CU02, Program: COUSR02C)
  - Delete users (Transaction: CU03, Program: COUSR03C)
- **System Administration**:
  - Access administrative menu (Transaction: CA00, Program: COADM01C)
  - Monitor system operations
  - Manage user security settings

**Access Restrictions**: Limited to administrative functions only, cannot perform regular user operations

## Overall Architecture

### Online CICS Components

The application uses a **3-tier CICS architecture**:

1. **Presentation Layer**: 3270 terminal interface using Basic Mapping Support (BMS)
   - Screen definitions in `app/bms/` directory
   - Interactive forms for data entry and display
   - Real-time user interaction

2. **Business Logic Layer**: COBOL programs handling transaction processing
   - Online transaction programs in `app/cbl/` directory
   - CICS program linkage using COMMAREA (COCOM01Y.cpy)
   - Real-time data validation and processing

3. **Data Access Layer**: VSAM file operations
   - High-performance indexed file access
   - Transaction integrity through CICS file control
   - Concurrent user access management

### Batch Processing Components

**Batch Architecture**: Sequential job processing using JCL
- **Job Control Language (JCL)**: Defines batch job execution sequence
- **COBOL Batch Programs**: Handle bulk data processing operations
- **File Processing**: Sequential and indexed file operations
- **Scheduling**: Dependent job execution based on return codes

### Key Architectural Patterns

1. **Transaction-Based Processing**: Each user interaction is a discrete CICS transaction
2. **Program Linkage**: Programs communicate through COMMAREA data structure
3. **File Sharing**: Online and batch components share VSAM data files
4. **Error Handling**: Comprehensive VSAM file status checking and error recovery
5. **Security Integration**: User authentication through VSAM security file (USRSEC)

## Technology Stack

### Core Technologies
- **COBOL**: Business logic implementation (Enterprise COBOL)
- **CICS**: Online transaction processing server
- **VSAM**: Virtual Storage Access Method for data storage
- **JCL**: Job Control Language for batch processing
- **BMS**: Basic Mapping Support for screen definitions

### Data Management
- **VSAM KSDS**: Key Sequenced Data Sets for master data
- **VSAM AIX**: Alternate Index for flexible data access
- **GDG**: Generation Data Groups for transaction history
- **EBCDIC**: Extended Binary Coded Decimal Interchange Code character encoding

### Integration Points
- **CICS-Batch Integration**: Shared VSAM files between online and batch
- **File Status Handling**: Comprehensive error detection and recovery
- **Transaction Context**: Maintains user session state across program calls
- **Security Framework**: Integrated user authentication and authorization

## Application Entry Points

### Online Access
- **Primary Transaction**: CC00 (Signon Screen)
- **Entry Program**: COSGN00C
- **Authentication**: VSAM USRSEC file validation
- **Navigation**: Context-sensitive menu system

### Batch Processing
- **Job Submission**: Through mainframe job scheduler
- **Execution Sequence**: Defined in JCL with conditional processing
- **Dependencies**: Sequential execution based on return codes
- **Monitoring**: Job completion status and error reporting

## Modernization Considerations

### Current State Challenges
- **Platform Dependency**: Requires mainframe infrastructure
- **Technology Skills**: COBOL and mainframe expertise needed
- **User Interface**: 3270 terminal interface limitations
- **Integration**: Limited API capabilities for modern applications

### Modernization Opportunities
- **Business Logic Preservation**: Core COBOL logic can be analyzed and transformed
- **Data Migration**: VSAM data can be converted to modern databases
- **User Interface**: 3270 screens can be modernized to web interfaces
- **API Development**: Transaction logic can be exposed as web services
- **Cloud Migration**: Application can be rehosted or refactored for cloud platforms

This application serves as an excellent reference for understanding mainframe application patterns and planning modernization strategies while preserving critical business functionality.
