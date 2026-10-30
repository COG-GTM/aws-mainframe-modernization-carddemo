//USRSECMG JOB 'MIGRATE USRSEC',REGION=8M,CLASS=A,
//      MSGCLASS=H,NOTIFY=&SYSUID
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
//* ONE-TIME MIGRATION OF AN EXISTING USRSEC KSDS FROM THE LEGACY
//* 80-BYTE LAYOUT (CLEAR-TEXT PASSWORD) TO THE 200-BYTE CSUSR01Y
//* LAYOUT (SALTED PBKDF2-HMAC-SHA256 HASH).
//* CLOSE FILE USRSEC IN CICS (CLOSEFIL) BEFORE RUNNING AND OPEN IT
//* AGAIN (OPENFIL) AFTERWARDS. RUN ONCE PER INSTALL.
//* THE HASHED RECORDS ARE KEPT IN AWS.M2.CARDDEMO.USRSEC.MIGR.PS AND
//* LOADED INTO A NEW CLUSTER; THE LIVE CLUSTER IS ONLY SWAPPED
//* (ALTER NEWNAME) AFTER A CLEAN LOAD. THE LEGACY CLUSTER IS KEPT AS
//* ...USRSEC.VSAM.KSDS.OLD80 FOR ROLLBACK. IT STILL HOLDS CLEAR-TEXT
//* PASSWORDS: RUN JOB USRSECCL AS SOON AS SIGN-ON HAS BEEN VERIFIED.
//* RESUBMITTING AFTER A FAILED LOAD IS SAFE: STEP00 THEN REMOVES THE
//* STAGED OUTPUT. IF ...KSDS.OLD80 EXISTS (SWAP DONE OR INTERRUPTED)
//* STEP00 ENDS WITH RC 16 AND NOTHING IS DELETED OR RUN; FINISH THE
//* ALTER NEWNAME SEQUENCE OF STEP03 BY HAND OR RUN USRSECCL.
//*-------------------------------------------------------------------*
//* REMOVE STAGED OUTPUT OF AN EARLIER FAILED RUN, UNLESS A SWAP HAS
//* ALREADY STARTED (OLD80 EXISTS)
//*-------------------------------------------------------------------*
//STEP00  EXEC PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
 LISTCAT ENTRIES(AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.OLD80)
 IF LASTCC = 0 -
   THEN SET MAXCC = 16
   ELSE DO
     DELETE AWS.M2.CARDDEMO.USRSEC.MIGR.PS NONVSAM
     DELETE AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.NEW CLUSTER
     SET MAXCC = 0
   END
/*
//*-------------------------------------------------------------------*
//* UNLOAD THE LEGACY 80-BYTE KSDS
//*-------------------------------------------------------------------*
//STEP01  EXEC PGM=IDCAMS
//IN       DD  DSN=AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS,DISP=OLD
//OUT      DD  DSN=&&USRIN,
//             DISP=(NEW,PASS,DELETE),
//             DCB=(LRECL=80,RECFM=FB,DSORG=PS,BLKSIZE=0),
//             UNIT=SYSAD,SPACE=(TRK,(10,5),RLSE)
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
  REPRO INFILE(IN) OUTFILE(OUT)
/*
//*-------------------------------------------------------------------*
//* HASH PASSWORDS INTO A NEW 200-BYTE PS FILE
//*-------------------------------------------------------------------*
//STEP02  EXEC PGM=CBUSR01C,COND=(0,NE)
//STEPLIB  DD DISP=SHR,
//         DSN=AWS.M2.CARDDEMO.LOADLIB
//USRIN    DD DSN=&&USRIN,DISP=(OLD,DELETE,DELETE)
//USROUT   DD DSN=AWS.M2.CARDDEMO.USRSEC.MIGR.PS,
//            DISP=(NEW,CATLG,DELETE),
//            DCB=(LRECL=200,RECFM=FB,DSORG=PS,BLKSIZE=0),
//            UNIT=SYSAD,SPACE=(TRK,(25,10),RLSE)
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*-------------------------------------------------------------------*
//* LOAD A NEW 200-BYTE KSDS, THEN SWAP IT IN FOR THE LEGACY CLUSTER
//*-------------------------------------------------------------------*
//STEP03  EXEC PGM=IDCAMS,COND=(0,NE)
//IN       DD  DSN=AWS.M2.CARDDEMO.USRSEC.MIGR.PS,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
 DEFINE    CLUSTER (NAME(AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.NEW)    -
                    KEYS(8,0)                                 -
                    RECORDSIZE(200,200)                       -
                    REUSE                                     -
                    INDEXED                                   -
                    TRACKS(115,40)                            -
                    FREESPACE(10,15)                          -
                    CISZ(8192))                               -
           DATA    (NAME(AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.NEW.DAT)) -
           INDEX   (NAME(AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.NEW.IDX))
 IF LASTCC = 0 THEN -
   REPRO INFILE(IN) OUTDATASET(AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.NEW)
 IF LASTCC = 0 THEN DO
   ALTER AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS -
         NEWNAME(AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.OLD80)
   ALTER AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.DAT -
         NEWNAME(AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.OLD80.DAT)
   ALTER AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.IDX -
         NEWNAME(AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.OLD80.IDX)
   ALTER AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.NEW -
         NEWNAME(AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS)
   ALTER AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.NEW.DAT -
         NEWNAME(AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.DAT)
   ALTER AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.NEW.IDX -
         NEWNAME(AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.IDX)
 END
/*
//
