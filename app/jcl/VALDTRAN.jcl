//VALDTRAN JOB 'VALDTRAN',CLASS=A,MSGCLASS=0,
// NOTIFY=&SYSUID
//* *******************************************************************
//* Pre-posting validation of the daily transaction file (CBTRN04C).
//* Reads DALYTRAN, writes the accepted feed to DALYVALD(+1), rejected
//* records with an 80-byte reason trailer to DALYRJ04(+1) and the
//* control-total report to VALDRPT. Nothing is posted by this job.
//*
//* HLQ     : high-level qualifier of the application data sets; set to
//*           the qualifier used by POSTTRAN.jcl for this site.
//* RUNDATE : run date YYYYMMDD passed as PARM, the same way INTCALC.jcl
//*           passes a date to CBACT04C. Transactions dated after it
//*           are rejected with reason 0209.
//* GDG bases DALYVALD and DALYRJ04 are defined by DALYRJ04.jcl.
//*
//* Return code: 0 no rejects, 4 some records rejected,
//*              8 PARM invalid, 12 file error.
//* *******************************************************************
// SET HLQ=SITE.HLQ
// SET RUNDATE=20220718
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
//*
