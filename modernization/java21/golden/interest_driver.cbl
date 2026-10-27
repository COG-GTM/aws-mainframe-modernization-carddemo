       >>source format free
identification division.
program-id. interest-driver.
data division.
working-storage section.
01 parms.
   05 parm-length binary-short signed value 10.
   05 parm-date pic x(10) value '2022071800'.
procedure division.
    call 'CBACT04C' using parms
    goback.
