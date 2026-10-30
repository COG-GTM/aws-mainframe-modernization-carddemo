      ******************************************************************
      * Program     : CSPWHASH.CBL
      * Application : CardDemo
      * Type        : Called subprogram (no CICS commands; batch+CICS)
      * Function    : Salted password hashing for the USRSEC file.
      *               Scheme 'P1' = PBKDF2-HMAC-SHA256 (RFC 8018),
      *               128-bit salt, 256-bit key, stored as hex text.
      *               Password and salt-entropy characters are hashed
      *               as their ASCII codes so a hash computed on an
      *               EBCDIC system verifies on an ASCII system.
      *               Parameter block: copybook CSPWH01Y.
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
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CSPWHASH.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-CONSTANTS.
         05 WS-SCHEME-PBKDF2           PIC X(02) VALUE 'P1'.
         05 WS-DEFAULT-ITERATIONS      PIC 9(06) VALUE 001000.
         05 WS-MAX-ITERATIONS          PIC 9(06) VALUE 100000.
         05 WS-HEX-LOWER               PIC X(16)
                                       VALUE '0123456789abcdef'.
         05 WS-HEX-UPPER               PIC X(16)
                                       VALUE '0123456789ABCDEF'.
         05 WS-TWO-POW-32              PIC 9(18) COMP-5
                                       VALUE 4294967296.
         05 WS-TWO-POW-31              PIC 9(18) COMP-5
                                       VALUE 2147483648.

      * Printable ASCII, code 32 (space) through code 126 (tilde)
       01 WS-ASCII-PRINTABLE.
         05 FILLER                     PIC X(32) VALUE
             ' !"#$%&''()*+,-./0123456789:;<=>?'.
         05 FILLER                     PIC X(32) VALUE
             '@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_'.
         05 FILLER                     PIC X(31) VALUE
             '`abcdefghijklmnopqrstuvwxyz{|}~'.
       01 WS-ASCII-TABLE REDEFINES WS-ASCII-PRINTABLE.
         05 WS-ASCII-CHAR              PIC X(01) OCCURS 95 TIMES.

      * SHA-256 round constants (FIPS 180-4 4.2.2)
       01 WS-K-VALUES.
         05 FILLER PIC 9(10) VALUE 1116352408.
         05 FILLER PIC 9(10) VALUE 1899447441.
         05 FILLER PIC 9(10) VALUE 3049323471.
         05 FILLER PIC 9(10) VALUE 3921009573.
         05 FILLER PIC 9(10) VALUE 0961987163.
         05 FILLER PIC 9(10) VALUE 1508970993.
         05 FILLER PIC 9(10) VALUE 2453635748.
         05 FILLER PIC 9(10) VALUE 2870763221.
         05 FILLER PIC 9(10) VALUE 3624381080.
         05 FILLER PIC 9(10) VALUE 0310598401.
         05 FILLER PIC 9(10) VALUE 0607225278.
         05 FILLER PIC 9(10) VALUE 1426881987.
         05 FILLER PIC 9(10) VALUE 1925078388.
         05 FILLER PIC 9(10) VALUE 2162078206.
         05 FILLER PIC 9(10) VALUE 2614888103.
         05 FILLER PIC 9(10) VALUE 3248222580.
         05 FILLER PIC 9(10) VALUE 3835390401.
         05 FILLER PIC 9(10) VALUE 4022224774.
         05 FILLER PIC 9(10) VALUE 0264347078.
         05 FILLER PIC 9(10) VALUE 0604807628.
         05 FILLER PIC 9(10) VALUE 0770255983.
         05 FILLER PIC 9(10) VALUE 1249150122.
         05 FILLER PIC 9(10) VALUE 1555081692.
         05 FILLER PIC 9(10) VALUE 1996064986.
         05 FILLER PIC 9(10) VALUE 2554220882.
         05 FILLER PIC 9(10) VALUE 2821834349.
         05 FILLER PIC 9(10) VALUE 2952996808.
         05 FILLER PIC 9(10) VALUE 3210313671.
         05 FILLER PIC 9(10) VALUE 3336571891.
         05 FILLER PIC 9(10) VALUE 3584528711.
         05 FILLER PIC 9(10) VALUE 0113926993.
         05 FILLER PIC 9(10) VALUE 0338241895.
         05 FILLER PIC 9(10) VALUE 0666307205.
         05 FILLER PIC 9(10) VALUE 0773529912.
         05 FILLER PIC 9(10) VALUE 1294757372.
         05 FILLER PIC 9(10) VALUE 1396182291.
         05 FILLER PIC 9(10) VALUE 1695183700.
         05 FILLER PIC 9(10) VALUE 1986661051.
         05 FILLER PIC 9(10) VALUE 2177026350.
         05 FILLER PIC 9(10) VALUE 2456956037.
         05 FILLER PIC 9(10) VALUE 2730485921.
         05 FILLER PIC 9(10) VALUE 2820302411.
         05 FILLER PIC 9(10) VALUE 3259730800.
         05 FILLER PIC 9(10) VALUE 3345764771.
         05 FILLER PIC 9(10) VALUE 3516065817.
         05 FILLER PIC 9(10) VALUE 3600352804.
         05 FILLER PIC 9(10) VALUE 4094571909.
         05 FILLER PIC 9(10) VALUE 0275423344.
         05 FILLER PIC 9(10) VALUE 0430227734.
         05 FILLER PIC 9(10) VALUE 0506948616.
         05 FILLER PIC 9(10) VALUE 0659060556.
         05 FILLER PIC 9(10) VALUE 0883997877.
         05 FILLER PIC 9(10) VALUE 0958139571.
         05 FILLER PIC 9(10) VALUE 1322822218.
         05 FILLER PIC 9(10) VALUE 1537002063.
         05 FILLER PIC 9(10) VALUE 1747873779.
         05 FILLER PIC 9(10) VALUE 1955562222.
         05 FILLER PIC 9(10) VALUE 2024104815.
         05 FILLER PIC 9(10) VALUE 2227730452.
         05 FILLER PIC 9(10) VALUE 2361852424.
         05 FILLER PIC 9(10) VALUE 2428436474.
         05 FILLER PIC 9(10) VALUE 2756734187.
         05 FILLER PIC 9(10) VALUE 3204031479.
         05 FILLER PIC 9(10) VALUE 3329325298.
       01 WS-K-TABLE REDEFINES WS-K-VALUES.
         05 WS-K-DISP                  PIC 9(10) OCCURS 64 TIMES.

      * SHA-256 initial hash value (FIPS 180-4 5.3.3)
       01 WS-IV-VALUES.
         05 FILLER PIC 9(10) VALUE 1779033703.
         05 FILLER PIC 9(10) VALUE 3144134277.
         05 FILLER PIC 9(10) VALUE 1013904242.
         05 FILLER PIC 9(10) VALUE 2773480762.
         05 FILLER PIC 9(10) VALUE 1359893119.
         05 FILLER PIC 9(10) VALUE 2600822924.
         05 FILLER PIC 9(10) VALUE 0528734635.
         05 FILLER PIC 9(10) VALUE 1541459225.
       01 WS-IV-TABLE REDEFINES WS-IV-VALUES.
         05 WS-IV-DISP                 PIC 9(10) OCCURS 8 TIMES.

       01 WS-INIT-FLAG                 PIC X(01) VALUE 'N'.
         88 WS-TABLES-READY                      VALUE 'Y'.

      * 4-bit XOR / AND lookup tables, index = (a * 16) + b + 1
       01 WS-LOOKUP-TABLES.
         05 WS-XOR-T                   PIC 9(04) COMP-5
                                       OCCURS 256 TIMES.
         05 WS-AND-T                   PIC 9(04) COMP-5
                                       OCCURS 256 TIMES.
         05 WS-POW2                    PIC 9(18) COMP-5
                                       OCCURS 33 TIMES.
         05 WS-K                       PIC 9(18) COMP-5
                                       OCCURS 64 TIMES.

       01 WS-SHA-STATE.
         05 WS-H                       PIC 9(18) COMP-5
                                       OCCURS 8 TIMES.
         05 WS-INNER-H                 PIC 9(18) COMP-5
                                       OCCURS 8 TIMES.
         05 WS-OUTER-H                 PIC 9(18) COMP-5
                                       OCCURS 8 TIMES.
         05 WS-DIGEST                  PIC 9(18) COMP-5
                                       OCCURS 8 TIMES.
         05 WS-U                       PIC 9(18) COMP-5
                                       OCCURS 8 TIMES.
         05 WS-T                       PIC 9(18) COMP-5
                                       OCCURS 8 TIMES.
         05 WS-SALT-WORD               PIC 9(18) COMP-5
                                       OCCURS 4 TIMES.
         05 WS-W                       PIC 9(18) COMP-5
                                       OCCURS 64 TIMES.

       01 WS-SHA-WORK.
         05 WS-VA                      PIC 9(18) COMP-5.
         05 WS-VB                      PIC 9(18) COMP-5.
         05 WS-VC                      PIC 9(18) COMP-5.
         05 WS-VD                      PIC 9(18) COMP-5.
         05 WS-VE                      PIC 9(18) COMP-5.
         05 WS-VF                      PIC 9(18) COMP-5.
         05 WS-VG                      PIC 9(18) COMP-5.
         05 WS-VH                      PIC 9(18) COMP-5.
         05 WS-T1                      PIC 9(18) COMP-5.
         05 WS-T2                      PIC 9(18) COMP-5.
         05 WS-S0                      PIC 9(18) COMP-5.
         05 WS-S1                      PIC 9(18) COMP-5.
         05 WS-SUM                     PIC 9(18) COMP-5.
         05 WS-Q                       PIC 9(18) COMP-5.
         05 WS-REM                     PIC 9(18) COMP-5.
         05 WS-X                       PIC 9(18) COMP-5.
         05 WS-R                       PIC 9(18) COMP-5.
         05 WS-N                       PIC 9(04) COMP-5.
         05 WS-OP-CODE                 PIC X(01).
           88 WS-OP-XOR3                         VALUE 'X'.
           88 WS-OP-CH                           VALUE 'C'.
           88 WS-OP-MAJ                          VALUE 'M'.
         05 WS-OP-A                    PIC 9(18) COMP-5.
         05 WS-OP-B                    PIC 9(18) COMP-5.
         05 WS-OP-C                    PIC 9(18) COMP-5.
         05 WS-OP-R                    PIC 9(18) COMP-5.
         05 WS-TA                      PIC 9(18) COMP-5.
         05 WS-TB                      PIC 9(18) COMP-5.
         05 WS-TC                      PIC 9(18) COMP-5.
         05 WS-MULT                    PIC 9(18) COMP-5.
         05 WS-NA                      PIC 9(04) COMP-5.
         05 WS-NB                      PIC 9(04) COMP-5.
         05 WS-NC                      PIC 9(04) COMP-5.
         05 WS-NR                      PIC 9(04) COMP-5.
         05 WS-N1                      PIC 9(04) COMP-5.
         05 WS-N2                      PIC 9(04) COMP-5.
         05 WS-N3                      PIC 9(04) COMP-5.
         05 WS-IX                      PIC 9(04) COMP-5.
         05 WS-BA                      PIC 9(04) COMP-5.
         05 WS-BB                      PIC 9(04) COMP-5.
         05 WS-BITV                    PIC 9(04) COMP-5.
         05 WS-XR                      PIC 9(04) COMP-5.
         05 WS-AR                      PIC 9(04) COMP-5.

       01 WS-MISC.
         05 WS-I                       PIC 9(04) COMP-5.
         05 WS-J                       PIC 9(04) COMP-5.
         05 WS-P                       PIC 9(04) COMP-5.
         05 WS-ITER                    PIC 9(08) COMP-5.
         05 WS-LEN                     PIC 9(04) COMP-5.
         05 WS-FOUND                   PIC 9(04) COMP-5.
         05 WS-BYTE                    PIC 9(04) COMP-5.
         05 WS-BYTE-R                  PIC 9(04) COMP-5.
         05 WS-PAD-HI                  PIC 9(04) COMP-5.
         05 WS-PAD-LO                  PIC 9(04) COMP-5.
         05 WS-CH                      PIC X(01).
         05 WS-HEX-OK                  PIC X(01).
           88 WS-HEX-VALID                       VALUE 'Y'.
         05 WS-TEXT                    PIC X(55).
         05 WS-HEX-OUT                 PIC X(64).
         05 WS-KEY-BYTE                PIC 9(04) COMP-5
                                       OCCURS 64 TIMES.
         05 WS-MSG-BYTE                PIC 9(04) COMP-5
                                       OCCURS 64 TIMES.

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       COPY CSPWH01Y.

      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION USING PWH-PARMS.
       MAIN-PARA.

           IF NOT WS-TABLES-READY
               PERFORM INIT-TABLES
           END-IF

           SET PWH-RC-OK TO TRUE

           EVALUATE TRUE
               WHEN PWH-FUNC-NEW
                   PERFORM NEW-PASSWORD-HASH
               WHEN PWH-FUNC-VERIFY
                   PERFORM VERIFY-PASSWORD-HASH
               WHEN OTHER
                   SET PWH-RC-BAD-INPUT TO TRUE
           END-EVALUATE

           GOBACK.

      *----------------------------------------------------------------*
      *  NEW-PASSWORD-HASH: salt = first 16 bytes of SHA-256(entropy)
      *----------------------------------------------------------------*
       NEW-PASSWORD-HASH.

           MOVE WS-SCHEME-PBKDF2      TO PWH-SCHEME
           MOVE WS-DEFAULT-ITERATIONS TO PWH-ITERATIONS
           MOVE SPACES                TO PWH-SALT
                                         PWH-HASH

           PERFORM LOAD-PASSWORD-KEY
           IF NOT PWH-RC-OK
               EXIT PARAGRAPH
           END-IF

           PERFORM DIGEST-ENTROPY
           MOVE WS-HEX-OUT(1:32) TO PWH-SALT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
               MOVE WS-DIGEST(WS-J) TO WS-SALT-WORD(WS-J)
           END-PERFORM

           MOVE PWH-ITERATIONS TO WS-ITER
           PERFORM PBKDF2-HMAC-SHA256
           MOVE WS-HEX-OUT TO PWH-HASH.

      *----------------------------------------------------------------*
      *  VERIFY-PASSWORD-HASH
      *----------------------------------------------------------------*
       VERIFY-PASSWORD-HASH.

           IF PWH-SCHEME NOT = WS-SCHEME-PBKDF2
               SET PWH-RC-BAD-SCHEME TO TRUE
               EXIT PARAGRAPH
           END-IF

           IF PWH-ITERATIONS IS NOT NUMERIC
              OR PWH-ITERATIONS = ZERO
              OR PWH-ITERATIONS > WS-MAX-ITERATIONS
               SET PWH-RC-BAD-INPUT TO TRUE
               EXIT PARAGRAPH
           END-IF

           PERFORM LOAD-PASSWORD-KEY
           IF NOT PWH-RC-OK
               EXIT PARAGRAPH
           END-IF

           PERFORM LOAD-SALT-WORDS
           IF NOT WS-HEX-VALID
               SET PWH-RC-BAD-INPUT TO TRUE
               EXIT PARAGRAPH
           END-IF

           MOVE PWH-ITERATIONS TO WS-ITER
           PERFORM PBKDF2-HMAC-SHA256

           IF WS-HEX-OUT = FUNCTION LOWER-CASE(PWH-HASH)
               SET PWH-RC-OK TO TRUE
           ELSE
               SET PWH-RC-MISMATCH TO TRUE
           END-IF.

      *----------------------------------------------------------------*
      *  LOAD-PASSWORD-KEY: password (trailing blanks dropped) as the
      *  HMAC key, zero padded to the 64-byte block, then the inner
      *  (key XOR 0x36) and outer (key XOR 0x5C) states are computed.
      *----------------------------------------------------------------*
       LOAD-PASSWORD-KEY.

           INSPECT PWH-PASSWORD REPLACING ALL LOW-VALUES BY SPACES
           MOVE 0 TO WS-LEN
           PERFORM VARYING WS-I FROM LENGTH OF PWH-PASSWORD BY -1
                   UNTIL WS-I < 1 OR WS-LEN > 0
               IF PWH-PASSWORD(WS-I:1) NOT = SPACE
                   MOVE WS-I TO WS-LEN
               END-IF
           END-PERFORM

           IF WS-LEN = 0
               SET PWH-RC-BAD-INPUT TO TRUE
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 64
               MOVE 0 TO WS-KEY-BYTE(WS-I)
           END-PERFORM
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN
               MOVE PWH-PASSWORD(WS-I:1) TO WS-CH
               PERFORM CHAR-TO-ASCII
               MOVE WS-BYTE TO WS-KEY-BYTE(WS-I)
           END-PERFORM

      *    inner pad 0x36
           MOVE 3 TO WS-PAD-HI
           MOVE 6 TO WS-PAD-LO
           PERFORM LOAD-PADDED-KEY-BLOCK
           PERFORM LOAD-IV
           PERFORM SHA256-COMPRESS
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8
               MOVE WS-H(WS-J) TO WS-INNER-H(WS-J)
           END-PERFORM

      *    outer pad 0x5C
           MOVE 5  TO WS-PAD-HI
           MOVE 12 TO WS-PAD-LO
           PERFORM LOAD-PADDED-KEY-BLOCK
           PERFORM LOAD-IV
           PERFORM SHA256-COMPRESS
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8
               MOVE WS-H(WS-J) TO WS-OUTER-H(WS-J)
           END-PERFORM.

       LOAD-PADDED-KEY-BLOCK.

           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 16
               MOVE 0 TO WS-W(WS-J)
               PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 4
                   COMPUTE WS-I = (WS-J - 1) * 4 + WS-P
                   MOVE WS-KEY-BYTE(WS-I) TO WS-BYTE
                   DIVIDE WS-BYTE BY 16 GIVING WS-NA REMAINDER WS-NB
                   COMPUTE WS-IX = WS-NA * 16 + WS-PAD-HI + 1
                   MOVE WS-XOR-T(WS-IX) TO WS-N1
                   COMPUTE WS-IX = WS-NB * 16 + WS-PAD-LO + 1
                   MOVE WS-XOR-T(WS-IX) TO WS-N2
                   COMPUTE WS-W(WS-J) = WS-W(WS-J) * 256
                                      + WS-N1 * 16 + WS-N2
               END-PERFORM
           END-PERFORM.

      *----------------------------------------------------------------*
      *  LOAD-SALT-WORDS: 32 hex characters -> 4 big-endian words
      *----------------------------------------------------------------*
       LOAD-SALT-WORDS.

           SET WS-HEX-VALID TO TRUE
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
               MOVE 0 TO WS-SALT-WORD(WS-J)
               PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 8
                   COMPUTE WS-I = (WS-J - 1) * 8 + WS-P
                   MOVE PWH-SALT(WS-I:1) TO WS-CH
                   MOVE 0 TO WS-FOUND
                   PERFORM VARYING WS-N FROM 1 BY 1
                           UNTIL WS-N > 16 OR WS-FOUND > 0
                       IF WS-CH = WS-HEX-LOWER(WS-N:1)
                          OR WS-CH = WS-HEX-UPPER(WS-N:1)
                           MOVE WS-N TO WS-FOUND
                       END-IF
                   END-PERFORM
                   IF WS-FOUND = 0
                       MOVE 'N' TO WS-HEX-OK
                       MOVE 1   TO WS-FOUND
                   END-IF
                   COMPUTE WS-SALT-WORD(WS-J) =
                           WS-SALT-WORD(WS-J) * 16 + WS-FOUND - 1
               END-PERFORM
           END-PERFORM.

      *----------------------------------------------------------------*
      *  DIGEST-ENTROPY: SHA-256 of PWH-ENTROPY (one block, <= 55
      *  bytes, trailing blanks dropped) -> WS-DIGEST and WS-HEX-OUT
      *----------------------------------------------------------------*
       DIGEST-ENTROPY.

           MOVE PWH-ENTROPY TO WS-TEXT
           INSPECT WS-TEXT REPLACING ALL LOW-VALUES BY SPACES
           MOVE 0 TO WS-LEN
           PERFORM VARYING WS-I FROM LENGTH OF WS-TEXT BY -1
                   UNTIL WS-I < 1 OR WS-LEN > 0
               IF WS-TEXT(WS-I:1) NOT = SPACE
                   MOVE WS-I TO WS-LEN
               END-IF
           END-PERFORM

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 64
               MOVE 0 TO WS-MSG-BYTE(WS-I)
           END-PERFORM
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN
               MOVE WS-TEXT(WS-I:1) TO WS-CH
               PERFORM CHAR-TO-ASCII
               MOVE WS-BYTE TO WS-MSG-BYTE(WS-I)
           END-PERFORM
           MOVE 128 TO WS-MSG-BYTE(WS-LEN + 1)

           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 15
               MOVE 0 TO WS-W(WS-J)
               PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 4
                   COMPUTE WS-I = (WS-J - 1) * 4 + WS-P
                   COMPUTE WS-W(WS-J) = WS-W(WS-J) * 256
                                      + WS-MSG-BYTE(WS-I)
               END-PERFORM
           END-PERFORM
           COMPUTE WS-W(16) = WS-LEN * 8

           PERFORM LOAD-IV
           PERFORM SHA256-COMPRESS
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8
               MOVE WS-H(WS-J) TO WS-DIGEST(WS-J)
           END-PERFORM
           PERFORM DIGEST-TO-HEX.

      *----------------------------------------------------------------*
      *  PBKDF2-HMAC-SHA256 with dkLen = 32 (a single output block):
      *    U1 = HMAC(P, salt || INT(1)), Ui = HMAC(P, Ui-1)
      *    T  = U1 XOR U2 XOR ... XOR Uc        -> WS-HEX-OUT
      *----------------------------------------------------------------*
       PBKDF2-HMAC-SHA256.

      *    message = salt (16 bytes) || 00000001, total 84 bytes
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
               MOVE WS-SALT-WORD(WS-J) TO WS-W(WS-J)
           END-PERFORM
           MOVE 1             TO WS-W(5)
           MOVE WS-TWO-POW-31 TO WS-W(6)
           PERFORM VARYING WS-J FROM 7 BY 1 UNTIL WS-J > 15
               MOVE 0 TO WS-W(WS-J)
           END-PERFORM
           MOVE 672           TO WS-W(16)
           PERFORM HMAC-BLOCK
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8
               MOVE WS-H(WS-J) TO WS-U(WS-J)
               MOVE WS-H(WS-J) TO WS-T(WS-J)
           END-PERFORM

           PERFORM UNTIL WS-ITER < 2
      *        message = previous U (32 bytes), total 96 bytes
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8
                   MOVE WS-U(WS-J) TO WS-W(WS-J)
               END-PERFORM
               MOVE WS-TWO-POW-31 TO WS-W(9)
               PERFORM VARYING WS-J FROM 10 BY 1 UNTIL WS-J > 15
                   MOVE 0 TO WS-W(WS-J)
               END-PERFORM
               MOVE 768           TO WS-W(16)
               PERFORM HMAC-BLOCK
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8
                   MOVE WS-H(WS-J) TO WS-U(WS-J)
                   MOVE WS-T(WS-J) TO WS-OP-A
                   MOVE WS-H(WS-J) TO WS-OP-B
                   MOVE 0          TO WS-OP-C
                   SET WS-OP-XOR3  TO TRUE
                   PERFORM NIBBLE-OP
                   MOVE WS-OP-R    TO WS-T(WS-J)
               END-PERFORM
               SUBTRACT 1 FROM WS-ITER
           END-PERFORM

           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8
               MOVE WS-T(WS-J) TO WS-DIGEST(WS-J)
           END-PERFORM
           PERFORM DIGEST-TO-HEX.

      *----------------------------------------------------------------*
      *  HMAC-BLOCK: WS-W(1..16) holds the padded final block of the
      *  inner message; result (8 words) is left in WS-H.
      *----------------------------------------------------------------*
       HMAC-BLOCK.

           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8
               MOVE WS-INNER-H(WS-J) TO WS-H(WS-J)
           END-PERFORM
           PERFORM SHA256-COMPRESS

           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8
               MOVE WS-H(WS-J) TO WS-W(WS-J)
               MOVE WS-OUTER-H(WS-J) TO WS-H(WS-J)
           END-PERFORM
           MOVE WS-TWO-POW-31 TO WS-W(9)
           PERFORM VARYING WS-J FROM 10 BY 1 UNTIL WS-J > 15
               MOVE 0 TO WS-W(WS-J)
           END-PERFORM
           MOVE 768 TO WS-W(16)
           PERFORM SHA256-COMPRESS.

      *----------------------------------------------------------------*
      *  SHA256-COMPRESS: one 512-bit block in WS-W(1..16), chaining
      *  value in WS-H(1..8) (FIPS 180-4 6.2.2)
      *----------------------------------------------------------------*
       SHA256-COMPRESS.

           PERFORM VARYING WS-I FROM 17 BY 1 UNTIL WS-I > 64
               MOVE WS-W(WS-I - 15) TO WS-X
               MOVE 7  TO WS-N
               PERFORM ROTR
               MOVE WS-R TO WS-OP-A
               MOVE 18 TO WS-N
               PERFORM ROTR
               MOVE WS-R TO WS-OP-B
               MOVE 3  TO WS-N
               PERFORM SHR
               MOVE WS-R TO WS-OP-C
               SET WS-OP-XOR3 TO TRUE
               PERFORM NIBBLE-OP
               MOVE WS-OP-R TO WS-S0

               MOVE WS-W(WS-I - 2) TO WS-X
               MOVE 17 TO WS-N
               PERFORM ROTR
               MOVE WS-R TO WS-OP-A
               MOVE 19 TO WS-N
               PERFORM ROTR
               MOVE WS-R TO WS-OP-B
               MOVE 10 TO WS-N
               PERFORM SHR
               MOVE WS-R TO WS-OP-C
               SET WS-OP-XOR3 TO TRUE
               PERFORM NIBBLE-OP
               MOVE WS-OP-R TO WS-S1

               COMPUTE WS-SUM = WS-W(WS-I - 16) + WS-S0
                              + WS-W(WS-I - 7)  + WS-S1
               DIVIDE WS-SUM BY WS-TWO-POW-32
                      GIVING WS-Q REMAINDER WS-W(WS-I)
           END-PERFORM

           MOVE WS-H(1) TO WS-VA
           MOVE WS-H(2) TO WS-VB
           MOVE WS-H(3) TO WS-VC
           MOVE WS-H(4) TO WS-VD
           MOVE WS-H(5) TO WS-VE
           MOVE WS-H(6) TO WS-VF
           MOVE WS-H(7) TO WS-VG
           MOVE WS-H(8) TO WS-VH

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 64
               MOVE WS-VE TO WS-X
               MOVE 6  TO WS-N
               PERFORM ROTR
               MOVE WS-R TO WS-OP-A
               MOVE 11 TO WS-N
               PERFORM ROTR
               MOVE WS-R TO WS-OP-B
               MOVE 25 TO WS-N
               PERFORM ROTR
               MOVE WS-R TO WS-OP-C
               SET WS-OP-XOR3 TO TRUE
               PERFORM NIBBLE-OP
               MOVE WS-OP-R TO WS-S1

               MOVE WS-VE TO WS-OP-A
               MOVE WS-VF TO WS-OP-B
               MOVE WS-VG TO WS-OP-C
               SET WS-OP-CH TO TRUE
               PERFORM NIBBLE-OP

               COMPUTE WS-SUM = WS-VH + WS-S1 + WS-OP-R
                              + WS-K(WS-I) + WS-W(WS-I)
               DIVIDE WS-SUM BY WS-TWO-POW-32
                      GIVING WS-Q REMAINDER WS-T1

               MOVE WS-VA TO WS-X
               MOVE 2  TO WS-N
               PERFORM ROTR
               MOVE WS-R TO WS-OP-A
               MOVE 13 TO WS-N
               PERFORM ROTR
               MOVE WS-R TO WS-OP-B
               MOVE 22 TO WS-N
               PERFORM ROTR
               MOVE WS-R TO WS-OP-C
               SET WS-OP-XOR3 TO TRUE
               PERFORM NIBBLE-OP
               MOVE WS-OP-R TO WS-S0

               MOVE WS-VA TO WS-OP-A
               MOVE WS-VB TO WS-OP-B
               MOVE WS-VC TO WS-OP-C
               SET WS-OP-MAJ TO TRUE
               PERFORM NIBBLE-OP

               COMPUTE WS-SUM = WS-S0 + WS-OP-R
               DIVIDE WS-SUM BY WS-TWO-POW-32
                      GIVING WS-Q REMAINDER WS-T2

               MOVE WS-VG TO WS-VH
               MOVE WS-VF TO WS-VG
               MOVE WS-VE TO WS-VF
               COMPUTE WS-SUM = WS-VD + WS-T1
               DIVIDE WS-SUM BY WS-TWO-POW-32
                      GIVING WS-Q REMAINDER WS-VE
               MOVE WS-VC TO WS-VD
               MOVE WS-VB TO WS-VC
               MOVE WS-VA TO WS-VB
               COMPUTE WS-SUM = WS-T1 + WS-T2
               DIVIDE WS-SUM BY WS-TWO-POW-32
                      GIVING WS-Q REMAINDER WS-VA
           END-PERFORM

           COMPUTE WS-SUM = WS-H(1) + WS-VA
           DIVIDE WS-SUM BY WS-TWO-POW-32 GIVING WS-Q REMAINDER WS-H(1)
           COMPUTE WS-SUM = WS-H(2) + WS-VB
           DIVIDE WS-SUM BY WS-TWO-POW-32 GIVING WS-Q REMAINDER WS-H(2)
           COMPUTE WS-SUM = WS-H(3) + WS-VC
           DIVIDE WS-SUM BY WS-TWO-POW-32 GIVING WS-Q REMAINDER WS-H(3)
           COMPUTE WS-SUM = WS-H(4) + WS-VD
           DIVIDE WS-SUM BY WS-TWO-POW-32 GIVING WS-Q REMAINDER WS-H(4)
           COMPUTE WS-SUM = WS-H(5) + WS-VE
           DIVIDE WS-SUM BY WS-TWO-POW-32 GIVING WS-Q REMAINDER WS-H(5)
           COMPUTE WS-SUM = WS-H(6) + WS-VF
           DIVIDE WS-SUM BY WS-TWO-POW-32 GIVING WS-Q REMAINDER WS-H(6)
           COMPUTE WS-SUM = WS-H(7) + WS-VG
           DIVIDE WS-SUM BY WS-TWO-POW-32 GIVING WS-Q REMAINDER WS-H(7)
           COMPUTE WS-SUM = WS-H(8) + WS-VH
           DIVIDE WS-SUM BY WS-TWO-POW-32 GIVING WS-Q REMAINDER WS-H(8).

      *----------------------------------------------------------------*
      *  ROTR / SHR of the 32-bit word WS-X by WS-N bits -> WS-R
      *----------------------------------------------------------------*
       ROTR.
           DIVIDE WS-X BY WS-POW2(WS-N + 1) GIVING WS-Q REMAINDER WS-REM
           COMPUTE WS-R = WS-Q + WS-REM * WS-POW2(33 - WS-N).

       SHR.
           DIVIDE WS-X BY WS-POW2(WS-N + 1) GIVING WS-R.

      *----------------------------------------------------------------*
      *  NIBBLE-OP: bitwise function of WS-OP-A/B/C -> WS-OP-R
      *    X: A XOR B XOR C   C: (A AND B) XOR (NOT A AND C)
      *    M: (A AND B) XOR (A AND C) XOR (B AND C)
      *----------------------------------------------------------------*
       NIBBLE-OP.
           MOVE WS-OP-A TO WS-TA
           MOVE WS-OP-B TO WS-TB
           MOVE WS-OP-C TO WS-TC
           MOVE 0 TO WS-OP-R
           MOVE 1 TO WS-MULT
           PERFORM 8 TIMES
               DIVIDE WS-TA BY 16 GIVING WS-TA REMAINDER WS-NA
               DIVIDE WS-TB BY 16 GIVING WS-TB REMAINDER WS-NB
               DIVIDE WS-TC BY 16 GIVING WS-TC REMAINDER WS-NC
               EVALUATE TRUE
                   WHEN WS-OP-XOR3
                       COMPUTE WS-IX = WS-NA * 16 + WS-NB + 1
                       MOVE WS-XOR-T(WS-IX) TO WS-N1
                       COMPUTE WS-IX = WS-N1 * 16 + WS-NC + 1
                       MOVE WS-XOR-T(WS-IX) TO WS-NR
                   WHEN WS-OP-CH
                       COMPUTE WS-IX = WS-NA * 16 + WS-NB + 1
                       MOVE WS-AND-T(WS-IX) TO WS-N1
                       COMPUTE WS-IX = (15 - WS-NA) * 16 + WS-NC + 1
                       MOVE WS-AND-T(WS-IX) TO WS-N2
                       COMPUTE WS-IX = WS-N1 * 16 + WS-N2 + 1
                       MOVE WS-XOR-T(WS-IX) TO WS-NR
                   WHEN OTHER
                       COMPUTE WS-IX = WS-NA * 16 + WS-NB + 1
                       MOVE WS-AND-T(WS-IX) TO WS-N1
                       COMPUTE WS-IX = WS-NA * 16 + WS-NC + 1
                       MOVE WS-AND-T(WS-IX) TO WS-N2
                       COMPUTE WS-IX = WS-NB * 16 + WS-NC + 1
                       MOVE WS-AND-T(WS-IX) TO WS-N3
                       COMPUTE WS-IX = WS-N1 * 16 + WS-N2 + 1
                       MOVE WS-XOR-T(WS-IX) TO WS-N1
                       COMPUTE WS-IX = WS-N1 * 16 + WS-N3 + 1
                       MOVE WS-XOR-T(WS-IX) TO WS-NR
               END-EVALUATE
               COMPUTE WS-OP-R = WS-OP-R + WS-NR * WS-MULT
               COMPUTE WS-MULT = WS-MULT * 16
           END-PERFORM.

      *----------------------------------------------------------------*
      *  CHAR-TO-ASCII: WS-CH -> ASCII code in WS-BYTE
      *----------------------------------------------------------------*
       CHAR-TO-ASCII.
           MOVE 0 TO WS-FOUND
           PERFORM VARYING WS-N FROM 1 BY 1
                   UNTIL WS-N > 95 OR WS-FOUND > 0
               IF WS-ASCII-CHAR(WS-N) = WS-CH
                   MOVE WS-N TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND > 0
               COMPUTE WS-BYTE = WS-FOUND + 31
           ELSE
               COMPUTE WS-BYTE = FUNCTION ORD(WS-CH) - 1
           END-IF.

      *----------------------------------------------------------------*
      *  DIGEST-TO-HEX: WS-DIGEST(1..8) -> 64 lower-case hex chars
      *----------------------------------------------------------------*
       DIGEST-TO-HEX.
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8
               MOVE WS-DIGEST(WS-J) TO WS-TA
               PERFORM VARYING WS-P FROM 8 BY -1 UNTIL WS-P < 1
                   DIVIDE WS-TA BY 16 GIVING WS-TA REMAINDER WS-NA
                   COMPUTE WS-I = (WS-J - 1) * 8 + WS-P
                   MOVE WS-HEX-LOWER(WS-NA + 1:1) TO WS-HEX-OUT(WS-I:1)
               END-PERFORM
           END-PERFORM.

       LOAD-IV.
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8
               MOVE WS-IV-DISP(WS-J) TO WS-H(WS-J)
           END-PERFORM.

      *----------------------------------------------------------------*
      *  INIT-TABLES: constants and 4-bit lookup tables (once per run
      *  unit / CICS task)
      *----------------------------------------------------------------*
       INIT-TABLES.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 64
               MOVE WS-K-DISP(WS-I) TO WS-K(WS-I)
           END-PERFORM

           MOVE 1 TO WS-POW2(1)
           PERFORM VARYING WS-I FROM 2 BY 1 UNTIL WS-I > 33
               COMPUTE WS-POW2(WS-I) = WS-POW2(WS-I - 1) * 2
           END-PERFORM

           PERFORM VARYING WS-NA FROM 0 BY 1 UNTIL WS-NA > 15
               PERFORM VARYING WS-NB FROM 0 BY 1 UNTIL WS-NB > 15
                   MOVE WS-NA TO WS-TA
                   MOVE WS-NB TO WS-TB
                   MOVE 0 TO WS-XR
                             WS-AR
                   MOVE 1 TO WS-BITV
                   PERFORM 4 TIMES
                       DIVIDE WS-TA BY 2 GIVING WS-TA REMAINDER WS-BA
                       DIVIDE WS-TB BY 2 GIVING WS-TB REMAINDER WS-BB
                       IF WS-BA NOT = WS-BB
                           ADD WS-BITV TO WS-XR
                       END-IF
                       IF WS-BA = 1 AND WS-BB = 1
                           ADD WS-BITV TO WS-AR
                       END-IF
                       COMPUTE WS-BITV = WS-BITV * 2
                   END-PERFORM
                   COMPUTE WS-IX = WS-NA * 16 + WS-NB + 1
                   MOVE WS-XR TO WS-XOR-T(WS-IX)
                   MOVE WS-AR TO WS-AND-T(WS-IX)
               END-PERFORM
           END-PERFORM

           SET WS-TABLES-READY TO TRUE.
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:33 CDT
      *
