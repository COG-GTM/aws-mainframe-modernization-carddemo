package com.carddemo.programs.coactupc;

import com.carddemo.programs.coactupc.support.Cobol;
import com.carddemo.programs.coactupc.support.ZonedDecimal;
import java.math.BigDecimal;

/**
 * ACCT-UPDATE-RECORD (working storage) - the 300-byte image REWRITten to ACCTDAT.
 * Unlike CVACT01Y it has no ACCT-ADDR-ZIP: ACCT-UPDATE-GROUP-ID sits where ACCT-ADDR-ZIP is in the
 * file layout and is followed by FILLER X(188).
 */
final class AcctUpdateRecord {

    long id;
    String activeStatus;
    BigDecimal currBal;
    BigDecimal creditLimit;
    BigDecimal cashCreditLimit;
    String openDate;
    String expiraionDate;
    String reissueDate;
    BigDecimal currCycCredit;
    BigDecimal currCycDebit;
    String groupId;

    AcctUpdateRecord() {
        initialize();
    }

    /** INITIALIZE ACCT-UPDATE-RECORD (FILLER is not touched; modelled as SPACES). */
    void initialize() {
        id = 0;
        activeStatus = Cobol.spaces(1);
        currBal = BigDecimal.ZERO;
        creditLimit = BigDecimal.ZERO;
        cashCreditLimit = BigDecimal.ZERO;
        openDate = Cobol.spaces(10);
        expiraionDate = Cobol.spaces(10);
        reissueDate = Cobol.spaces(10);
        currCycCredit = BigDecimal.ZERO;
        currCycDebit = BigDecimal.ZERO;
        groupId = Cobol.spaces(10);
    }

    String toFixedWidth() {
        return Cobol.unsignedDisplay(id, 11)
                + Cobol.pad(activeStatus, 1)
                + ZonedDecimal.encode(currBal, 10, 2)
                + ZonedDecimal.encode(creditLimit, 10, 2)
                + ZonedDecimal.encode(cashCreditLimit, 10, 2)
                + Cobol.pad(openDate, 10)
                + Cobol.pad(expiraionDate, 10)
                + Cobol.pad(reissueDate, 10)
                + ZonedDecimal.encode(currCycCredit, 10, 2)
                + ZonedDecimal.encode(currCycDebit, 10, 2)
                + Cobol.pad(groupId, 10)
                + Cobol.spaces(188);
    }
}
