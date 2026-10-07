# Dependency-order check against docs/modernization/dependency-map.json

Run order: ACCTFILE -> CARDFILE -> CUSTFILE -> XREFFILE -> TRANFILE -> TRANTYPE -> TRANCATG -> DISCGRP -> TCATBALF -> DUSRSECJ -> READACCT -> READCARD -> READCUST -> READXREF -> CBTRN01C -> POSTTRAN -> INTCALC -> TRANBKP -> COMBTRAN -> TRANREPT -> CREASTMT -> PRTCATBL -> CBEXPORT -> CBIMPORT -> WAITSTEP -> CSUTLDTC

- READACCT is listed as depending on INTCALC (via AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS) but runs before it. The map derives edges from shared datasets, so jobs that both read and update a KSDS depend on each other; the baseline follows the ticket/JCL flow instead (print jobs against the freshly loaded masters, POSTTRAN before INTCALC, TRANBKP before COMBTRAN before TRANREPT).
- READACCT is listed as depending on POSTTRAN (via AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS) but runs before it. The map derives edges from shared datasets, so jobs that both read and update a KSDS depend on each other; the baseline follows the ticket/JCL flow instead (print jobs against the freshly loaded masters, POSTTRAN before INTCALC, TRANBKP before COMBTRAN before TRANREPT).
- POSTTRAN is listed as depending on INTCALC (via AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS) but runs before it. The map derives edges from shared datasets, so jobs that both read and update a KSDS depend on each other; the baseline follows the ticket/JCL flow instead (print jobs against the freshly loaded masters, POSTTRAN before INTCALC, TRANBKP before COMBTRAN before TRANREPT).
- TRANBKP is listed as depending on COMBTRAN (via AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS) but runs before it. The map derives edges from shared datasets, so jobs that both read and update a KSDS depend on each other; the baseline follows the ticket/JCL flow instead (print jobs against the freshly loaded masters, POSTTRAN before INTCALC, TRANBKP before COMBTRAN before TRANREPT).
- TRANBKP: upstream TRANIDX is not part of the baseline run (not a batch job runnable here).
- COMBTRAN: upstream DEFGDGB is not part of the baseline run (not a batch job runnable here).
- COMBTRAN is listed as depending on TRANREPT (via AWS.M2.CARDDEMO.TRANSACT.BKUP) but runs before it. The map derives edges from shared datasets, so jobs that both read and update a KSDS depend on each other; the baseline follows the ticket/JCL flow instead (print jobs against the freshly loaded masters, POSTTRAN before INTCALC, TRANBKP before COMBTRAN before TRANREPT).
- TRANREPT: upstream TRANIDX is not part of the baseline run (not a batch job runnable here).
- TRANREPT: upstream DEFGDGB is not part of the baseline run (not a batch job runnable here).
- CREASTMT: upstream TRANIDX is not part of the baseline run (not a batch job runnable here).
- PRTCATBL: upstream DEFGDGB is not part of the baseline run (not a batch job runnable here).
- CBEXPORT: upstream TRANIDX is not part of the baseline run (not a batch job runnable here).

Result: all other recorded dependencies run earlier.
