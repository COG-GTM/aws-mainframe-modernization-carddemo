package com.carddemo.programs.cocrdupc;

/**
 * Request DTO for EXEC CICS RECEIVE MAP('CCRDUPA') MAPSET('COCRDUP') INTO(CCRDUPAI). A null
 * field means "not transmitted by the terminal" (BMS fills the I field with LOW-VALUES).
 * Transmitted values are padded per the BMS JUSTIFY option (EXPMON/EXPYEAR/EXPDAY are
 * JUSTIFY=RIGHT, the rest default LEFT,BLANK).
 */
public record CcrdupaInput(
        String acctsid,  // ACCTSIDI X(11)
        String cardsid,  // CARDSIDI X(16)
        String crdname,  // CRDNAMEI X(50)
        String crdstcd,  // CRDSTCDI X(1)
        String expmon,   // EXPMONI  X(2)
        String expyear,  // EXPYEARI X(4)
        String expday    // EXPDAYI  X(2)
) {
    public static CcrdupaInput search(String acct, String card) {
        return new CcrdupaInput(acct, card, null, null, null, null, null);
    }

    public static CcrdupaInput none() {
        return new CcrdupaInput(null, null, null, null, null, null, null);
    }
}
