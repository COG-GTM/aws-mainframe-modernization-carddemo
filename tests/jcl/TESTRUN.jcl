//TESTRUN  JOB 'Run CardDemo Unit Tests',CLASS=A,MSGCLASS=H,
//         MSGLEVEL=(1,1),NOTIFY=&SYSUID,TIME=1440
//******************************************************************
//* Copyright Amazon.com, Inc. or its affiliates.
//* All Rights Reserved.
//*
//* Licensed under the Apache License, Version 2.0 (the "License").
//* You may not use this file except in compliance with the License.
//* You may obtain a copy of the License at
//*
//*    http://www.apache.org/licenses/LICENSE-2.0
//*
//* Unless required by applicable law or agreed to in writing,
//* software distributed under the License is distributed on an
//* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
//* either express or implied. See the License for the specific
//* language governing permissions and limitations under the License
//******************************************************************
//* Runs the CardDemo unit test drivers as batch steps.
//* Each driver DISPLAYs PASS/FAIL per assertion plus a summary and
//* sets RETURN-CODE to the number of failed assertions, so any
//* step ending with RC > 0 means test failures (check SYSOUT).
//*
//* Compile the drivers first with tests/jcl/TESTCMPL.jcl.
//*
//* To collect code coverage on z/OS, run these steps under IBM
//* Debug for z/OS code coverage (EQANMDBG) or from IBM Developer
//* for z/OS / zUnit; see docs/TESTING.md.
//******************************************************************
//* STEP010: define the VSAM KSDS test files used by TSTSTM1C
//******************************************************************
//STEP010  EXEC PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
  DELETE    AWS.M2.CARDDEMO.UTEST.TRNXFILE CLUSTER
  DELETE    AWS.M2.CARDDEMO.UTEST.XREFFILE CLUSTER
  DELETE    AWS.M2.CARDDEMO.UTEST.CUSTFILE CLUSTER
  DELETE    AWS.M2.CARDDEMO.UTEST.ACCTFILE CLUSTER
  SET       MAXCC = 0
  DEFINE    CLUSTER  (NAME(AWS.M2.CARDDEMO.UTEST.TRNXFILE)      -
                      KEYS(32 0)                                -
                      RECORDSIZE(350 350)                       -
                      SHAREOPTIONS(2 3)                         -
                      INDEXED                                   -
                      TRK(1 1))
  DEFINE    CLUSTER  (NAME(AWS.M2.CARDDEMO.UTEST.XREFFILE)      -
                      KEYS(16 0)                                -
                      RECORDSIZE(50 50)                         -
                      SHAREOPTIONS(2 3)                         -
                      INDEXED                                   -
                      TRK(1 1))
  DEFINE    CLUSTER  (NAME(AWS.M2.CARDDEMO.UTEST.CUSTFILE)      -
                      KEYS(9 0)                                 -
                      RECORDSIZE(500 500)                       -
                      SHAREOPTIONS(2 3)                         -
                      INDEXED                                   -
                      TRK(1 1))
  DEFINE    CLUSTER  (NAME(AWS.M2.CARDDEMO.UTEST.ACCTFILE)      -
                      KEYS(11 0)                                -
                      RECORDSIZE(300 300)                       -
                      SHAREOPTIONS(2 3)                         -
                      INDEXED                                   -
                      TRK(1 1))
/*
//******************************************************************
//* STEP020: load the test fixture records
//******************************************************************
//STEP020  EXEC PGM=STMFIXTC,COND=(0,NE)
//STEPLIB  DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.LOADLIB
//TRNXFILE DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.UTEST.TRNXFILE
//XREFFILE DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.UTEST.XREFFILE
//CUSTFILE DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.UTEST.CUSTFILE
//ACCTFILE DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.UTEST.ACCTFILE
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//******************************************************************
//* STEP030: date validation tests for CSUTLDTC (uses LE CEEDAYS)
//******************************************************************
//STEP030  EXEC PGM=TSTDTC1C,COND=(0,NE)
//STEPLIB  DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.LOADLIB
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//******************************************************************
//* STEP040: date edit tests for the CSUTLDPY copybook logic
//******************************************************************
//STEP040  EXEC PGM=TSTDTE1C,COND=(0,NE)
//STEPLIB  DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.LOADLIB
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//******************************************************************
//* STEP050: file access tests for the CBSTM03B subroutine
//******************************************************************
//STEP050  EXEC PGM=TSTSTM1C,COND=(0,NE)
//STEPLIB  DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.LOADLIB
//TRNXFILE DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.UTEST.TRNXFILE
//XREFFILE DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.UTEST.XREFFILE
//CUSTFILE DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.UTEST.CUSTFILE
//ACCTFILE DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.UTEST.ACCTFILE
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
