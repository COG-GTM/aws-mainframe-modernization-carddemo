//POSTTRN2 JOB 'POSTTRN2',CLASS=A,MSGCLASS=0,
// NOTIFY=&SYSUID
//******************************************************************
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
//* *******************************************************************
//* Modified copy of POSTTRAN.jcl with a pre-posting validation step.
//*
//* STEP10 CBTRN04C validates the daily transaction file and writes the
//*        accepted feed to DALYVALD(+1), rejects to DALYRJ04(+1) and
//*        the control-total report to VALDRPT.
//* STEP12 IEFBR14 runs only when STEP10 ended above 4 and deletes the
//*        two generations STEP10 cataloged, so a failed validation
//*        leaves nothing behind.
//* STEP15 CBTRN02C posts the transactions, reading the validated feed
//*        DALYVALD(+1) in place of the raw DALYTRAN file. Everything
//*        else in STEP15 is as in POSTTRAN.jcl.
//*
//* COND=(4,LT,STEP10) bypasses STEP15 when 4 < RC(STEP10), i.e. when
//* the validation step ended above 4 (PARM or file error). RC 4 (some
//* records rejected) still posts the clean feed; RC 0 posts everything.
//*
//* HLQ     : high-level qualifier of the application data sets; set to
//*           the qualifier used by POSTTRAN.jcl for this site.
//* RUNDATE : run date YYYYMMDD passed to CBTRN04C as PARM, the same
//*           way INTCALC.jcl passes a date to CBACT04C.
//* GDG bases DALYVALD and DALYRJ04 are defined by DALYRJ04.jcl.
//* POSTTRAN.jcl is left unchanged; swapping it for this job is a
//* system-owner decision (docs/sustainment/cbtrn04c).
//* *******************************************************************
// SET HLQ=SITE.HLQ
// SET RUNDATE=20220718
//* *******************************************************************
//* Step 1: pre-posting validation
//* *******************************************************************
//STEP10 EXEC PGM=CBTRN04C,PARM='&RUNDATE'
//STEPLIB  DD DISP=SHR,
//            DSN=&HLQ..LOADLIB
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//VALDRPT  DD SYSOUT=*,
//         DCB=(RECFM=F,LRECL=133)
//DALYTRAN DD DISP=SHR,
//         DSN=&HLQ..DALYTRAN.PS
//TRANTYPE DD DISP=SHR,
//         DSN=&HLQ..TRANTYPE.VSAM.KSDS
//TRANCATG DD DISP=SHR,
//         DSN=&HLQ..TRANCATG.VSAM.KSDS
//XREFFILE DD DISP=SHR,
//         DSN=&HLQ..CARDXREF.VSAM.KSDS
//ACCTFILE DD DISP=SHR,
//         DSN=&HLQ..ACCTDATA.VSAM.KSDS
//TCATBALF DD DISP=SHR,
//         DSN=&HLQ..TCATBALF.VSAM.KSDS
//DALYVALD DD DISP=(NEW,CATLG,DELETE),
//         UNIT=SYSDA,
//         DCB=(RECFM=F,LRECL=350,BLKSIZE=0),
//         SPACE=(CYL,(1,1),RLSE),
//         DSN=&HLQ..DALYVALD(+1)
//DALYRJ04 DD DISP=(NEW,CATLG,DELETE),
//         UNIT=SYSDA,
//         DCB=(RECFM=F,LRECL=430,BLKSIZE=0),
//         SPACE=(CYL,(1,1),RLSE),
//         DSN=&HLQ..DALYRJ04(+1)
//* *******************************************************************
//* Cleanup after a failed validation. STEP10 ends normally with RC 8
//* or 12 on a PARM or file error, so DISP=(NEW,CATLG,DELETE) has
//* already cataloged the two new generations, possibly empty or
//* partial. COND=(4,GE,STEP10) bypasses this step when 4 >= RC(STEP10)
//* (RC 0 or 4, a good run); on RC 8 or 12 it deletes DALYVALD(+1) and
//* DALYRJ04(+1) so the failed cycle leaves no generation behind and
//* the next run's (+1) is the first one after the last good feed.
//* *******************************************************************
//STEP12 EXEC PGM=IEFBR14,COND=(4,GE,STEP10)
//DALYVALD DD DISP=(MOD,DELETE,DELETE),
//         UNIT=SYSDA,SPACE=(TRK,0),
//         DSN=&HLQ..DALYVALD(+1)
//DALYRJ04 DD DISP=(MOD,DELETE,DELETE),
//         UNIT=SYSDA,SPACE=(TRK,0),
//         DSN=&HLQ..DALYRJ04(+1)
//* *******************************************************************
//* Step 2: post the validated feed (POSTTRAN.jcl STEP15, DALYTRAN now
//* points at the DALYVALD generation created by STEP10)
//* *******************************************************************
//STEP15 EXEC PGM=CBTRN02C,COND=(4,LT,STEP10)
//STEPLIB  DD DISP=SHR,
//            DSN=&HLQ..LOADLIB
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//TRANFILE DD DISP=SHR,
//         DSN=&HLQ..TRANSACT.VSAM.KSDS
//DALYTRAN DD DISP=SHR,
//         DSN=&HLQ..DALYVALD(+1)
//XREFFILE DD DISP=SHR,
//         DSN=&HLQ..CARDXREF.VSAM.KSDS
//DALYREJS DD DISP=(NEW,CATLG,DELETE),
//         UNIT=SYSDA,
//         DCB=(RECFM=F,LRECL=430,BLKSIZE=0),
//         SPACE=(CYL,(1,1),RLSE),
//         DSN=&HLQ..DALYREJS(+1)
//ACCTFILE DD DISP=SHR,
//         DSN=&HLQ..ACCTDATA.VSAM.KSDS
//TCATBALF DD DISP=SHR,
//         DSN=&HLQ..TCATBALF.VSAM.KSDS
//*
