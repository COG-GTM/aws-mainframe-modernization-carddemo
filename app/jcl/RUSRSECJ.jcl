//RUSRSECJ JOB 'RESET USRSEC USERS',REGION=8M,CLASS=A,
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
//*-------------------------------------------------------------------*
//* RESET USER PASSWORDS IN THE EXISTING USRSEC KSDS (RECOVERY)
//*
//* Use when accounts are locked at sign-on because their stored
//* password is a published default or fails the password policy
//* (e.g. ADMIN001-ADMIN005 loaded by older versions of DUSRSECJ) and
//* no administrator can sign on to reset them.
//*
//* Prepare AWS.M2.CARDDEMO.USRSEC.BOOTSTRAP exactly as for DUSRSECJ
//* (protected, FB 80, layout CSUSR01Y) with one record per account to
//* reset, each with a new random one-time password, including at
//* least one administrator. CBUSR01C validates every record; if any
//* fails the job ends RC=12 and the KSDS is not touched. Otherwise
//* REPRO REPLACE overwrites only those user IDs (adding any that do
//* not exist) and every other account is kept. Reset accounts must
//* change their password at first sign-on. Delete the bootstrap
//* dataset afterwards.
//*-------------------------------------------------------------------*
//*
//STEP01  EXEC PGM=CBUSR01C
//STEPLIB  DD DISP=SHR,
//            DSN=AWS.M2.CARDDEMO.LOADLIB
//BOOTIN   DD DISP=SHR,
//            DSN=AWS.M2.CARDDEMO.USRSEC.BOOTSTRAP
//USROUT   DD DSN=&&USRSEC,
//            DISP=(NEW,PASS,DELETE),
//            DCB=(LRECL=80,RECFM=FB,DSORG=PS,BLKSIZE=0),
//            UNIT=SYSAD,SPACE=(TRK,(10,5),RLSE)
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*
//*-------------------------------------------------------------------*
//* REPLACE THE VALIDATED RECORDS IN THE EXISTING USER SECURITY KSDS
//*-------------------------------------------------------------------*
//*
//STEP02  EXEC PGM=IDCAMS,COND=(4,LT,STEP01)
//*
//IN       DD  DSN=&&USRSEC,DISP=(OLD,DELETE,DELETE)
//OUT      DD  DSN=AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS,DISP=OLD
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
  REPRO INFILE(IN) OUTFILE(OUT) REPLACE
/*
//
