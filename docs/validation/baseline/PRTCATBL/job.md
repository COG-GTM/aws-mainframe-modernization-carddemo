# PRTCATBL

JCL: `app/jcl/PRTCATBL.jcl`
- STEP05 IDCAMS REPRO TCATBALF.VSAM.KSDS -> TCATBALF.BKUP(+1) (emulated: IDXTCATB UNLD).
- STEP10 SORT: `SORT FIELDS=(1,11,ZD,A,12,2,CH,A,14,4,ZD,A)` `OUTREC FIELDS=(1,11,X,12,2,X,14,4,X,18,11,ZD,EDIT=(TTTTTTTTT.TT),9X)` (emulated in Python). The reformatted record is 41 bytes while the JCL SORTOUT DCB says LRECL=40; the baseline keeps the 41-byte OUTREC layout and flags the JCL discrepancy.

## Outputs
- `TCATBALF.BKUP.txt`: 100 records x LRECL 50
- `TCATBALF.REPT.txt`: 100 records x LRECL 41
