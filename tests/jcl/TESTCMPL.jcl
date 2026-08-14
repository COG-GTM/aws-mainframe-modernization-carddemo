//TESTCMPL JOB 'Compile CardDemo Unit Tests',CLASS=A,MSGCLASS=H,
//             MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID,TIME=1440
//*********************************************************************
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
//*********************************************************************
//****  Compile the CardDemo unit test drivers as batch COBOL     ****
//****  programs, using the same BUILDBAT procedure as the        ****
//****  application programs (see samples/jcl/BATCMP.jcl).        ****
//****                                                            ****
//****  The test sources in tests/cobol and tests/fixtures must   ****
//****  be uploaded to the &HLQ..CARDDEMO.CBL PDS and the test    ****
//****  copybooks in tests/cpy to the &HLQ..CARDDEMO.CPY PDS      ****
//****  before running this job.                                  ****
//****                                                            ****
//****  NOTE: do NOT compile tests/stubs/CEEDAYS.cbl on z/OS.     ****
//****  The stub only replaces the LE date service for            ****
//****  off-mainframe runs; on z/OS the real CEEDAYS is used.     ****
//****                                                            ****
//****  Check with your Administrator for JCL suitable to your    ****
//****  environment.                                              ****
//*********************************************************************
//   SET HLQ=AWS.M2
//*********************************************************************
//*  Add proclib reference
//*********************************************************************
//CCLIBS  JCLLIB ORDER=&HLQ..CARDDEMO.PRC.UTIL
//*********************************************************************
//*  Compile the test fixture and the unit test drivers
//*********************************************************************
//CMPFIXT  EXEC BUILDBAT,MEM=STMFIXTC,HLQ=&HLQ
//CMPDTC1  EXEC BUILDBAT,MEM=TSTDTC1C,HLQ=&HLQ
//CMPDTE1  EXEC BUILDBAT,MEM=TSTDTE1C,HLQ=&HLQ
//CMPSTM1  EXEC BUILDBAT,MEM=TSTSTM1C,HLQ=&HLQ
