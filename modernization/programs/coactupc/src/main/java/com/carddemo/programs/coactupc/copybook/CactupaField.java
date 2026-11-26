package com.carddemo.programs.coactupc.copybook;

/**
 * COACTUP (cpy-bms/COACTUP.CPY) - fields of map CACTUPA in mapset COACTUP, in symbolic-map order,
 * with the length of each xxxxI / xxxxO data item.
 */
public enum CactupaField {
    TRNNAME(4), TITLE01(40), CURDATE(8), PGMNAME(8), TITLE02(40), CURTIME(8),
    ACCTSID(11), ACSTTUS(1),
    OPNYEAR(4), OPNMON(2), OPNDAY(2),
    ACRDLIM(15),
    EXPYEAR(4), EXPMON(2), EXPDAY(2),
    ACSHLIM(15),
    RISYEAR(4), RISMON(2), RISDAY(2),
    ACURBAL(15), ACRCYCR(15), AADDGRP(10), ACRCYDB(15),
    ACSTNUM(9), ACTSSN1(3), ACTSSN2(2), ACTSSN3(4),
    DOBYEAR(4), DOBMON(2), DOBDAY(2),
    ACSTFCO(3), ACSFNAM(25), ACSMNAM(25), ACSLNAM(25),
    ACSADL1(50), ACSSTTE(2), ACSADL2(50), ACSZIPC(5), ACSCITY(50), ACSCTRY(3),
    ACSPH1A(3), ACSPH1B(3), ACSPH1C(4), ACSGOVT(20), ACSPH2A(3), ACSPH2B(3), ACSPH2C(4),
    ACSEFTC(10), ACSPFLG(1),
    INFOMSG(45), ERRMSG(78), FKEYS(21), FKEY05(7), FKEY12(10);

    public final int length;

    CactupaField(int length) {
        this.length = length;
    }
}
