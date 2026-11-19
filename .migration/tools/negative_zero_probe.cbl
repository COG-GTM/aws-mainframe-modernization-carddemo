       IDENTIFICATION DIVISION.
       PROGRAM-ID. NZ.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SRC   PIC S9(09)V99.
       01 SRCX  REDEFINES SRC PIC X(11).
       01 DST   PIC S9(09)V99.
       01 DSTX  REDEFINES DST PIC X(11).
       01 BAL10 PIC S9(10)V99.
       01 BAL10X REDEFINES BAL10 PIC X(12).
       PROCEDURE DIVISION.
           MOVE '0000000000}' TO SRCX
           MOVE SRC TO DST
           DISPLAY 'MOVE -0          ' DSTX
           MOVE 0 TO DST
           ADD SRC TO DST
           DISPLAY 'ADD -0 TO +0     ' DSTX
           MOVE '0000000000}' TO DSTX
           ADD SRC TO DST
           DISPLAY 'ADD -0 TO -0     ' DSTX
           MOVE '0000000000}' TO DSTX
           MOVE 1 TO SRC
           ADD SRC TO DST
           DISPLAY 'ADD +1 TO -0     ' DSTX
           MOVE -999999999.99 TO DST
           MOVE -0.01 TO SRC
           ADD SRC TO DST
           DISPLAY 'OVF NEG TO ZERO  ' DSTX
           MOVE 999999999.99 TO DST
           MOVE 0.01 TO SRC
           ADD SRC TO DST
           DISPLAY 'OVF POS TO ZERO  ' DSTX
           MOVE 5 TO DST
           MOVE -5 TO SRC
           ADD SRC TO DST
           DISPLAY '5 + -5           ' DSTX
           MOVE -9999999999.99 TO BAL10
           MOVE -0.01 TO SRC
           ADD SRC TO BAL10
           DISPLAY 'OVF10 NEG        ' BAL10X
           INITIALIZE DST
           MOVE '0000000000}' TO SRCX
           ADD SRC TO DST
           DISPLAY 'INIT + -0        ' DSTX
           MOVE '0000000000}' TO SRCX
           IF SRC >= 0 DISPLAY '-0 >= 0 TRUE' END-IF
           STOP RUN.
