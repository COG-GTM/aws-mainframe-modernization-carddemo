package com.carddemo.programs.coactupc;

import com.carddemo.programs.coactupc.support.Cobol;

/** CUST-UPDATE-RECORD (working storage) - the 500-byte image REWRITten to CUSTDAT (CVCUS01Y layout). */
final class CustUpdateRecord {

    long id;
    String firstName;
    String middleName;
    String lastName;
    String addrLine1;
    String addrLine2;
    String addrLine3;
    String addrStateCd;
    String addrCountryCd;
    String addrZip;
    String phoneNum1;
    String phoneNum2;
    long ssn;
    String govtIssuedId;
    String dobYyyyMmDd;
    String eftAccountId;
    String priCardInd;
    long ficoCreditScore;

    CustUpdateRecord() {
        initialize();
    }

    /** INITIALIZE CUST-UPDATE-RECORD (FILLER is not touched; modelled as SPACES). */
    void initialize() {
        id = 0;
        firstName = Cobol.spaces(25);
        middleName = Cobol.spaces(25);
        lastName = Cobol.spaces(25);
        addrLine1 = Cobol.spaces(50);
        addrLine2 = Cobol.spaces(50);
        addrLine3 = Cobol.spaces(50);
        addrStateCd = Cobol.spaces(2);
        addrCountryCd = Cobol.spaces(3);
        addrZip = Cobol.spaces(10);
        phoneNum1 = Cobol.spaces(15);
        phoneNum2 = Cobol.spaces(15);
        ssn = 0;
        govtIssuedId = Cobol.spaces(20);
        dobYyyyMmDd = Cobol.spaces(10);
        eftAccountId = Cobol.spaces(10);
        priCardInd = Cobol.spaces(1);
        ficoCreditScore = 0;
    }

    String toFixedWidth() {
        return Cobol.unsignedDisplay(id, 9)
                + Cobol.pad(firstName, 25) + Cobol.pad(middleName, 25) + Cobol.pad(lastName, 25)
                + Cobol.pad(addrLine1, 50) + Cobol.pad(addrLine2, 50) + Cobol.pad(addrLine3, 50)
                + Cobol.pad(addrStateCd, 2) + Cobol.pad(addrCountryCd, 3) + Cobol.pad(addrZip, 10)
                + Cobol.pad(phoneNum1, 15) + Cobol.pad(phoneNum2, 15)
                + Cobol.unsignedDisplay(ssn, 9)
                + Cobol.pad(govtIssuedId, 20) + Cobol.pad(dobYyyyMmDd, 10) + Cobol.pad(eftAccountId, 10)
                + Cobol.pad(priCardInd, 1)
                + Cobol.unsignedDisplay(ficoCreditScore, 3)
                + Cobol.spaces(168);
    }
}
