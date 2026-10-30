//CBEXPORT JOB 'Export Customer Data for Migration',CLASS=A,MSGCLASS=0,
// NOTIFY=&SYSUID
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
//* *******************************************************************
//* EXPORT CUSTOMER DATA FROM VSAM FILES TO MULTI-RECORD EXPORT FILE
//* FOR BRANCH MIGRATION OR DATA TRANSFER PURPOSES
//* *******************************************************************
//* DATA PROTECTION: THE EXPORT HOLDS CARD NUMBERS, EXPIRY DATES, SSNS
//* AND GOVERNMENT IDS (CVV CODES ARE NEVER EXPORTED). THE CLUSTER IS
//* DEFINED WITH Z/OS DATA SET ENCRYPTION, SO STORAGE, BACKUP AND
//* DFSMSDSS DUMP COPIES HOLD ONLY CIPHERTEXT. BEFORE THE FIRST RUN
//* THE SECURITY ADMINISTRATOR MUST:
//*  1. CREATE AN AES-256 CKDS KEY WITH LABEL CARDDEMO.MIGRATION.EXPKEY
//*     AND SHARE IT ONLY WITH THE RECEIVING SITE'S IMPORT JOB.
//*  2. DEFINE DATA CLASS CDMIGENC WITH EXTENDED FORMAT (REQUIRED FOR
//*     DATA SET ENCRYPTION).
//*  3. LIMIT THE KEY AND THE DATA SET TO THE MIGRATION JOB USER IDS:
//*     RDEFINE CSFKEYS CARDDEMO.MIGRATION.EXPKEY UACC(NONE) -
//*       ICSF(SYMCPACFWRAP(YES) SYMCPACFRET(YES))
//*     PERMIT CARDDEMO.MIGRATION.EXPKEY CLASS(CSFKEYS) -
//*       ID(<EXPORT/IMPORT JOB IDS>) ACCESS(READ)
//*     SETROPTS RACLIST(CSFKEYS) REFRESH
//*     ADDSD 'AWS.M2.CARDDEMO.EXPORT.**' UACC(NONE)
//*     PERMIT 'AWS.M2.CARDDEMO.EXPORT.**' ID(<EXPORT JOB ID>) -
//*       ACCESS(ALTER)
//*     PERMIT 'AWS.M2.CARDDEMO.EXPORT.**' ID(<IMPORT JOB ID>) -
//*       ACCESS(READ)
//* THE LISTCAT IN STEP01 MUST SHOW DATA SET ENCRYPTION...(YES).
//* *******************************************************************
//* STEP 1: DEFINE VSAM CLUSTER FOR EXPORT FILE
//* *******************************************************************
//STEP01   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE AWS.M2.CARDDEMO.EXPORT.DATA CLUSTER PURGE
  SET MAXCC = 0
  
  DEFINE CLUSTER (NAME(AWS.M2.CARDDEMO.EXPORT.DATA) -
                  INDEXED -
                  KEYS(4 28) -
                  RECORDSIZE(500 500) -
                  CYLINDERS(10 5) -
                  FREESPACE(10 10) -
                  SHAREOPTIONS(2 3) -
                  DATACLASS(CDMIGENC) -
                  KEYLABEL(CARDDEMO.MIGRATION.EXPKEY)) -
         DATA (NAME(AWS.M2.CARDDEMO.EXPORT.DATA.DATA)) -
         INDEX (NAME(AWS.M2.CARDDEMO.EXPORT.DATA.INDEX))

  LISTCAT ENTRIES(AWS.M2.CARDDEMO.EXPORT.DATA) ALL
/*
//* *******************************************************************
//* STEP 2: RUN EXPORT PROGRAM
//* *******************************************************************
//STEP02 EXEC PGM=CBEXPORT
//STEPLIB  DD DISP=SHR,
//         DSN=AWS.M2.CARDDEMO.LOADLIB
//*
//* INPUT VSAM FILES
//*
//CUSTFILE DD DISP=SHR,
//         DSN=AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS
//ACCTFILE DD DISP=SHR,
//         DSN=AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS
//XREFFILE DD DISP=SHR,
//         DSN=AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS
//TRANSACT DD DISP=SHR,
//         DSN=AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS
//CARDFILE DD DISP=SHR,
//         DSN=AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS
//*
//* OUTPUT EXPORT VSAM FILE
//*
//EXPFILE  DD DISP=SHR,
//         DSN=AWS.M2.CARDDEMO.EXPORT.DATA
//*
//* SYSTEM OUTPUT
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*
//*
//* Ver: CardDemo_v2.0-44-gb6e9c27-254 Date: 2025-10-16 14:07:18 CDT
//*
