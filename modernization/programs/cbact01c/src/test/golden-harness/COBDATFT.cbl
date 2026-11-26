       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBDATFT.
       DATA DIVISION.
       LINKAGE SECTION.
       01 REC.
          05 INTYPE  PIC X.
          05 INPDT   PIC X(20).
          05 OUTYPE  PIC X.
          05 OUTDT   PIC X(20).
          05 ERMSG   PIC X(38).
       PROCEDURE DIVISION USING REC.
           EVALUATE INTYPE
             WHEN '1'
               IF INPDT(5:1) = '-' OR OUTYPE = '2'
                  MOVE 'INVALID INPUT' TO ERMSG
               ELSE
                  MOVE INPDT(1:4) TO OUTDT(1:4)
                  MOVE '-' TO OUTDT(5:1)
                  MOVE INPDT(5:2) TO OUTDT(6:2)
                  MOVE '-' TO OUTDT(8:1)
                  MOVE INPDT(7:2) TO OUTDT(9:2)
               END-IF
             WHEN '2'
               IF OUTYPE = '1'
                  MOVE 'INVALID INPUT' TO ERMSG
               ELSE
                  MOVE INPDT(1:4) TO OUTDT(1:4)
                  MOVE INPDT(6:2) TO OUTDT(5:2)
                  MOVE INPDT(9:2) TO OUTDT(7:2)
               END-IF
             WHEN OTHER
               MOVE 'INVALID INPUT' TO ERMSG
           END-EVALUATE
           GOBACK.
