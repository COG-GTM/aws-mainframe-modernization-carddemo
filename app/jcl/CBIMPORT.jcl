//CBIMPORT JOB 'Import CARDDEMO Data',CLASS=A,MSGCLASS=0,               
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
//* IMPORT CUSTOMER DATA FROM MULTI-RECORD EXPORT FILE AND SPLIT
//* INTO SEPARATE NORMALIZED FILES FOR TARGET SYSTEM
//* *******************************************************************
//* CBIMPORT VALIDATES EVERY RECORD FIRST. IF ANY RECORD IS REJECTED
//* THE STEP ENDS WITH RC=8, THE .IMPORT LOAD FILES ARE LEFT EMPTY
//* AND THE REJECTS ARE LISTED IN AWS.M2.CARDDEMO.IMPORT.ERRORS.
//* DO NOT LOAD THE .IMPORT FILES UNLESS STEP02 ENDS WITH RC=0.
//* *******************************************************************
//*
//* WORK KSDS OF KEYS SEEN IN THE EXPORT FILE (DUPLICATE/REFERENCE
//* CHECKS). RE-DEFINED ON EVERY RUN.
//*
//STEP01   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE AWS.M2.CARDDEMO.IMPORT.KEYWORK CLUSTER PURGE
  SET MAXCC = 0

  DEFINE CLUSTER (NAME(AWS.M2.CARDDEMO.IMPORT.KEYWORK) -
                  INDEXED -
                  KEYS(17 0) -
                  RECORDSIZE(50 50) -
                  CYLINDERS(5 5) -
                  FREESPACE(10 10) -
                  SHAREOPTIONS(1 3)) -
         DATA (NAME(AWS.M2.CARDDEMO.IMPORT.KEYWORK.DATA)) -
         INDEX (NAME(AWS.M2.CARDDEMO.IMPORT.KEYWORK.INDEX))
/*
//* HLQ OF THE DESTINATION ENVIRONMENT'S LIVE VSAM CLUSTERS - CHANGE ME
//         SET DEST=CHANGE.ME.DEST
//STEP02 EXEC PGM=CBIMPORT,COND=(0,NE)
//STEPLIB  DD DISP=SHR,
//         DSN=AWS.M2.CARDDEMO.LOADLIB
//*
//* INPUT EXPORT FILE
//*
//EXPFILE  DD DISP=SHR,
//         DSN=AWS.M2.CARDDEMO.EXPORT.DATA
//*
//* VALIDATION WORK FILE
//*
//KEYWORK  DD DISP=OLD,
//         DSN=AWS.M2.CARDDEMO.IMPORT.KEYWORK
//*
//* LIVE TARGET FILES (READ ONLY) OF THE *DESTINATION* ENVIRONMENT.
//* REQUIRED: AN IMPORTED KEY THAT ALREADY EXISTS HERE IS REJECTED AND
//* A MISSING DD FAILS THE IMPORT. SET DEST (SEE STEP02) TO THE HLQ OF
//* THE DESTINATION CLUSTERS - NEVER THE CLUSTERS THE EXPORT WAS TAKEN
//* FROM, OR EVERY RECORD IS REJECTED.
//*
//CUSTFILE DD DISP=SHR,
//         DSN=&DEST..CUSTDATA.VSAM.KSDS
//ACCTFILE DD DISP=SHR,
//         DSN=&DEST..ACCTDATA.VSAM.KSDS
//XREFFILE DD DISP=SHR,
//         DSN=&DEST..CARDXREF.VSAM.KSDS
//TRANSACT DD DISP=SHR,
//         DSN=&DEST..TRANSACT.VSAM.KSDS
//CARDFILE DD DISP=SHR,
//         DSN=&DEST..CARDDATA.VSAM.KSDS
//*
//* OUTPUT NORMALIZED FILES
//*
//CUSTOUT  DD DISP=(NEW,CATLG,DELETE),
//         DSN=AWS.M2.CARDDEMO.CUSTDATA.IMPORT,
//         UNIT=SYSDA,
//         SPACE=(TRK,(50,25),RLSE),
//         DCB=(RECFM=FB,LRECL=500,BLKSIZE=0)
//ACCTOUT  DD DISP=(NEW,CATLG,DELETE),
//         DSN=AWS.M2.CARDDEMO.ACCTDATA.IMPORT,
//         UNIT=SYSDA,
//         SPACE=(TRK,(50,25),RLSE),
//         DCB=(RECFM=FB,LRECL=300,BLKSIZE=0)
//XREFOUT  DD DISP=(NEW,CATLG,DELETE),
//         DSN=AWS.M2.CARDDEMO.CARDXREF.IMPORT,
//         UNIT=SYSDA,
//         SPACE=(TRK,(25,10),RLSE),
//         DCB=(RECFM=FB,LRECL=50,BLKSIZE=0)
//TRNXOUT  DD DISP=(NEW,CATLG,DELETE),
//         DSN=AWS.M2.CARDDEMO.TRANSACT.IMPORT,
//         UNIT=SYSDA,
//         SPACE=(TRK,(100,50),RLSE),
//         DCB=(RECFM=FB,LRECL=350,BLKSIZE=0)
//CARDOUT  DD DISP=(NEW,CATLG,DELETE),
//         DSN=AWS.M2.CARDDEMO.CARDDATA.IMPORT,
//         UNIT=SYSDA,
//         SPACE=(TRK,(25,10),RLSE),
//         DCB=(RECFM=FB,LRECL=150,BLKSIZE=0)
//*
//* ERROR OUTPUT FILE
//*
//ERROUT   DD DISP=(NEW,CATLG,DELETE),
//         DSN=AWS.M2.CARDDEMO.IMPORT.ERRORS,
//         UNIT=SYSDA,
//         SPACE=(TRK,(10,5),RLSE),
//         DCB=(RECFM=FB,LRECL=132,BLKSIZE=0)
//*
//* SYSTEM OUTPUT
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*//*
//* Ver: CardDemo_v2.0-41-g02a5b3e-251 Date: 2025-10-02 13:43:17 CDT
//*
