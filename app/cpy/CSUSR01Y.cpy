      ******************************************************************
      * Copyright Amazon.com, Inc. or its affiliates.                   
      * All Rights Reserved.                                            
      *                                                                 
      * Licensed under the Apache License, Version 2.0 (the "License"). 
      * You may not use this file except in compliance with the License.
      * You may obtain a copy of the License at                         
      *                                                                 
      *    http://www.apache.org/licenses/LICENSE-2.0                   
      *                                                                 
      * Unless required by applicable law or agreed to in writing,      
      * software distributed under the License is distributed on an     
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    
      * either express or implied. See the License for the specific     
      * language governing permissions and limitations under the License
      ****************************************************************** 
      * Passwords are never stored. SEC-USR-PWD-HASH holds a salted
      * PBKDF2-HMAC-SHA256 hash built and checked by CSPWHASH (see
      * copybook CSPWH01Y). The original 8-byte password slot is kept
      * as SEC-USR-PWD-RSVD (always spaces) so that bytes 1-80 keep
      * their legacy offsets. Record length is 200 (USRSEC KSDS/PS).
       01 SEC-USER-DATA.
         05 SEC-USR-ID                 PIC X(08).
         05 SEC-USR-FNAME              PIC X(20).
         05 SEC-USR-LNAME              PIC X(20).
         05 SEC-USR-PWD-RSVD           PIC X(08).
         05 SEC-USR-TYPE               PIC X(01).
         05 SEC-USR-FILLER             PIC X(23).
         05 SEC-USR-PWD-HASH.
           10 SEC-USR-PWD-SCHEME       PIC X(02).
           10 SEC-USR-PWD-ITERATIONS   PIC 9(06).
           10 SEC-USR-PWD-SALT         PIC X(32).
           10 SEC-USR-PWD-DIGEST       PIC X(64).
         05 SEC-USR-FILLER2            PIC X(16).
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:15:59 CDT
      *
