//DALYRJ04 JOB 'DEF GDG FOR VALD',CLASS=A,MSGCLASS=0,NOTIFY=&SYSUID
//* *******************************************************************
//* Define the GDG bases used by VALDTRAN.jcl and POSTTRN2.jcl:
//*   DALYVALD  accepted daily transactions (350 bytes) from CBTRN04C
//*   DALYRJ04  rejected daily transactions + reason trailer (430 bytes)
//* Same shape as DALYREJS.jcl. LIMIT(5) mirrors that job; the retention
//* the site needs is a system-owner decision (docs/sustainment/cbtrn04c).
//* HLQ: set to the qualifier used by POSTTRAN.jcl for this site.
//* *******************************************************************
// SET HLQ=SITE.HLQ
//STEP05 EXEC PGM=IDCAMS
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   *,SYMBOLS=JCLONLY
   DEFINE GENERATIONDATAGROUP -
   (NAME(&HLQ..DALYVALD) -
    LIMIT(5) -
    SCRATCH -
   )
   DEFINE GENERATIONDATAGROUP -
   (NAME(&HLQ..DALYRJ04) -
    LIMIT(5) -
    SCRATCH -
   )
/*
