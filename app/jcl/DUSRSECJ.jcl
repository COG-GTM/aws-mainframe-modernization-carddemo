//DUSRSECJ JOB 'DEF USRSEC FILE',REGION=8M,CLASS=A,
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
//* PRE DELETE STEP
//*-------------------------------------------------------------------*
//*
//PREDEL  EXEC PGM=IEFBR14
//*
//DD01     DD DSN=AWS.M2.CARDDEMO.USRSEC.PS,
//            DISP=(MOD,DELETE,DELETE)
//*
//*-------------------------------------------------------------------*
//* VALIDATE THE BOOTSTRAP USER FILE AND BUILD THE USRSEC LOAD FILE
//*
//* No user IDs or passwords are shipped with this job. Before running
//* it, the security administrator creates the bootstrap dataset
//* AWS.M2.CARDDEMO.USRSEC.BOOTSTRAP (FB, LRECL=80, layout CSUSR01Y)
//* holding ONE administrator record with a randomly generated,
//* one-time password, and protects it so only the administrator and
//* the batch user ID can read it, e.g.
//*   ADDSD  'AWS.M2.CARDDEMO.USRSEC.BOOTSTRAP' UACC(NONE)
//*   PERMIT 'AWS.M2.CARDDEMO.USRSEC.BOOTSTRAP' ID(batchid) ACCESS(READ)
//* CBUSR01C rejects the whole file (RC=12, nothing loaded, existing
//* USRSEC left in place) if any password is blank, shorter than 8,
//* equal to the user ID, a published default such as PASSWORD, or
//* lacks letters and digits. Every loaded account must change its
//* password at first sign-on. Delete the bootstrap dataset after the
//* administrator has signed on. Never load demo/test users with
//* known passwords into a non-demo region.
//*-------------------------------------------------------------------*
//*
//STEP01  EXEC PGM=CBUSR01C
//STEPLIB  DD DISP=SHR,
//            DSN=AWS.M2.CARDDEMO.LOADLIB
//BOOTIN   DD DISP=SHR,
//            DSN=AWS.M2.CARDDEMO.USRSEC.BOOTSTRAP
//USROUT   DD DSN=AWS.M2.CARDDEMO.USRSEC.PS,
//            DISP=(NEW,CATLG,DELETE),
//            DCB=(LRECL=80,RECFM=FB,DSORG=PS,BLKSIZE=0),
//            UNIT=SYSAD,SPACE=(TRK,(10,5),RLSE)
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//*
//*-------------------------------------------------------------------*
//* DEFINE VSAM FILE FOR USER SECURITY
//*-------------------------------------------------------------------*
//*
//STEP02  EXEC PGM=IDCAMS,COND=(4,LT,STEP01)
//*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
 DELETE                  AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS
 SET       MAXCC = 0
 DEFINE    CLUSTER (NAME(AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS)    -
                    KEYS(8,0)                                 -
                    RECORDSIZE(80,80)                         -
                    REUSE                                     -
                    INDEXED                                   -
                    TRACKS(45,15)                             -
                    FREESPACE(10,15)                          -
                    CISZ(8192))                               -
           DATA    (NAME(AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.DAT)) -
           INDEX   (NAME(AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.IDX))
/*
//*
//*-------------------------------------------------------------------*
//* COPY USER SECURITY DATA FROM PS TO VSAM FILE
//*-------------------------------------------------------------------*
//*
//STEP03  EXEC PGM=IDCAMS,COND=(4,LT)
//*
//IN       DD  DSN=AWS.M2.CARDDEMO.USRSEC.PS,DISP=SHR
//OUT      DD  DSN=AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS,DISP=SHR
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
  REPRO INFILE(IN) OUTFILE(OUT)
/*
//
//*
//* Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:23:06 CDT
//*
