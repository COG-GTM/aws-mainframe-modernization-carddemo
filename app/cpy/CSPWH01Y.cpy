      ******************************************************************
      * CardDemo - Parameter block for CSPWHASH (password hashing)
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
      * PWH-FUNC-NEW    : build a new salt from PWH-ENTROPY and hash
      *                   PWH-PASSWORD; returns scheme, iterations,
      *                   salt and hash to be stored in USRSEC.
      * PWH-FUNC-VERIFY : hash PWH-PASSWORD with the stored scheme,
      *                   iterations and salt and compare with PWH-HASH.
      ******************************************************************
       01 PWH-PARMS.
         05 PWH-FUNCTION               PIC X(01).
           88 PWH-FUNC-NEW                       VALUE 'N'.
           88 PWH-FUNC-VERIFY                    VALUE 'V'.
         05 PWH-RETURN-CODE            PIC 9(02).
           88 PWH-RC-OK                          VALUE 00.
           88 PWH-RC-MISMATCH                    VALUE 04.
           88 PWH-RC-BAD-INPUT                   VALUE 08.
           88 PWH-RC-BAD-SCHEME                  VALUE 12.
         05 PWH-PASSWORD               PIC X(32).
         05 PWH-ENTROPY                PIC X(55).
         05 PWH-SCHEME                 PIC X(02).
         05 PWH-ITERATIONS             PIC 9(06).
         05 PWH-SALT                   PIC X(32).
         05 PWH-HASH                   PIC X(64).
