       IDENTIFICATION DIVISION.
       PROGRAM-ID. DLISTUB.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEGF ASSIGN TO 'segs.bin' ORGANIZATION IS SEQUENTIAL.
           SELECT STF ASSIGN TO 'state.txt' ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SEGF RECORD CONTAINS 201 CHARACTERS.
       01 SEG-REC.
          05 SEG-TYPE PIC X.
          05 SEG-DATA PIC X(200).
       FD STF.
       01 ST-LINE PIC X(80).
       WORKING-STORAGE SECTION.
       01 LOADED PIC X VALUE 'N'.
       01 EOF-F PIC X VALUE 'N'.
       01 NS PIC 9(4) COMP VALUE 0.
       01 ND PIC 9(4) COMP VALUE 0.
       01 CSX PIC 9(4) COMP VALUE 0.
       01 CDX PIC 9(4) COMP VALUE 0.
       01 I PIC 9(4) COMP.
       01 J PIC 9(4) COMP.
       01 SUMS.
          05 S-ENT OCCURS 100.
             10 S-DEL PIC X.
             10 S-FIRST PIC 9(4) COMP.
             10 S-LAST PIC 9(4) COMP.
             10 S-DATA PIC X(100).
       01 DTLS.
          05 D-ENT OCCURS 1000.
             10 D-DEL PIC X.
             10 D-DATA PIC X(200).
       01 KEYS.
          05 K-ACCT PIC S9(11) COMP-3.
          05 FILLER PIC X(94).
       01 DKEY.
          05 K-DATE PIC S9(5) COMP-3.
          05 K-TIME PIC S9(9) COMP-3.
          05 FILLER PIC X(192).
       01 OUTL.
          05 O-T PIC X(2).
          05 O-A PIC -9(11).
          05 FILLER PIC X VALUE ' '.
          05 O-D PIC -9(5).
          05 FILLER PIC X VALUE ' '.
          05 O-TM PIC -9(9).
       LINKAGE SECTION.
       01 L-FUNC PIC X(4).
       01 L-SEG PIC X(8).
       01 L-AREA PIC X(200).
       01 L-STAT PIC X(2).
       PROCEDURE DIVISION USING L-FUNC L-SEG L-AREA L-STAT.
           IF LOADED = 'N' PERFORM LOAD-DB END-IF
           MOVE SPACES TO L-STAT
           EVALUATE L-FUNC ALSO L-SEG
             WHEN 'GN  ' ALSO 'PAUTSUM0'
               ADD 1 TO CSX
               PERFORM UNTIL CSX > NS OR S-DEL(CSX) = 'N'
                 ADD 1 TO CSX
               END-PERFORM
               IF CSX > NS
                 MOVE 'GB' TO L-STAT
               ELSE
                 MOVE S-DATA(CSX) TO L-AREA(1:100)
                 COMPUTE CDX = S-FIRST(CSX) - 1
               END-IF
             WHEN 'GNP ' ALSO 'PAUTDTL1'
               ADD 1 TO CDX
               PERFORM UNTIL CDX > S-LAST(CSX) OR D-DEL(CDX) = 'N'
                 ADD 1 TO CDX
               END-PERFORM
               IF CDX > S-LAST(CSX)
                 MOVE 'GE' TO L-STAT
                 MOVE S-LAST(CSX) TO CDX
               ELSE
                 MOVE D-DATA(CDX) TO L-AREA
               END-IF
             WHEN 'DLET' ALSO 'PAUTDTL1'
               MOVE 'Y' TO D-DEL(CDX)
             WHEN 'DLET' ALSO 'PAUTSUM0'
               MOVE 'Y' TO S-DEL(CSX)
               PERFORM VARYING J FROM S-FIRST(CSX) BY 1 UNTIL J > S-LAST(CSX)
                 MOVE 'Y' TO D-DEL(J)
               END-PERFORM
             WHEN 'CHKP' ALSO ANY
               PERFORM DUMP-STATE
           END-EVALUATE
           GOBACK.
       LOAD-DB.
           MOVE 'Y' TO LOADED
           OPEN INPUT SEGF
           PERFORM UNTIL EOF-F = 'Y'
             READ SEGF AT END MOVE 'Y' TO EOF-F
             NOT AT END
               IF SEG-TYPE = 'S'
                 ADD 1 TO NS
                 MOVE 'N' TO S-DEL(NS)
                 MOVE SEG-DATA(1:100) TO S-DATA(NS)
                 COMPUTE S-FIRST(NS) = ND + 1
                 MOVE ND TO S-LAST(NS)
               ELSE
                 ADD 1 TO ND
                 MOVE 'N' TO D-DEL(ND)
                 MOVE SEG-DATA TO D-DATA(ND)
                 MOVE ND TO S-LAST(NS)
               END-IF
             END-READ
           END-PERFORM
           CLOSE SEGF.
       DUMP-STATE.
           OPEN OUTPUT STF
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NS
             IF S-DEL(I) = 'N'
               MOVE S-DATA(I) TO KEYS
               MOVE SPACES TO OUTL
               MOVE 'S ' TO O-T
               MOVE K-ACCT TO O-A
               MOVE ZERO TO O-D O-TM
               WRITE ST-LINE FROM OUTL
               PERFORM VARYING J FROM S-FIRST(I) BY 1 UNTIL J > S-LAST(I)
                 IF D-DEL(J) = 'N'
                   MOVE D-DATA(J) TO DKEY
                   MOVE 'D ' TO O-T
                   MOVE K-DATE TO O-D
                   MOVE K-TIME TO O-TM
                   WRITE ST-LINE FROM OUTL
                 END-IF
               END-PERFORM
             END-IF
           END-PERFORM
           CLOSE STF.
