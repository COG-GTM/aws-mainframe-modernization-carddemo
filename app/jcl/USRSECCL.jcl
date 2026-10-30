//USRSECCL JOB 'CLEANUP USRSEC',REGION=8M,CLASS=A,
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
//* CLEANUP AFTER USRSECMG. RUN ONLY AFTER USRSEC HAS BEEN REOPENED
//* IN CICS AND SIGN-ON WITH THE MIGRATED (HASHED) RECORDS HAS BEEN
//* VERIFIED. DELETES THE LEGACY CLUSTER, WHICH STILL HOLDS CLEAR-TEXT
//* PASSWORDS, AND THE MIGRATION PS FILE. ENDS WITH RC 16 WITHOUT
//* DELETING ANYTHING UNLESS THE USRSECMG SWAP IS COMPLETE (LIVE
//* CLUSTER PRESENT, ...KSDS.NEW GONE). SAFE TO RERUN.
//******************************************************************
//STEP01  EXEC PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
 LISTCAT ENTRIES(AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS)
 IF LASTCC NE 0 -
   THEN SET MAXCC = 16
   ELSE DO
     LISTCAT ENTRIES(AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.NEW)
     IF LASTCC = 0 -
       THEN SET MAXCC = 16
       ELSE DO
         SET MAXCC = 0
         DELETE AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS.OLD80 CLUSTER
         DELETE AWS.M2.CARDDEMO.USRSEC.MIGR.PS NONVSAM
       END
   END
/*
//
