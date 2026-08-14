      ******************************************************************
      *Working Storage Copybook for CardDemo unit test harness
      *Accompanying Procedure Division copybook is TSTASPY
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
         05  WS-TEST-NAME                          PIC X(60).
         05  WS-TESTS-RUN                          PIC 9(4) VALUE 0.
         05  WS-TESTS-PASSED                       PIC 9(4) VALUE 0.
         05  WS-TESTS-FAILED                       PIC 9(4) VALUE 0.
