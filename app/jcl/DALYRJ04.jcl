//DALYRJ04 JOB 'DEF GDG FOR VALD',CLASS=A,MSGCLASS=0,NOTIFY=&SYSUID
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
