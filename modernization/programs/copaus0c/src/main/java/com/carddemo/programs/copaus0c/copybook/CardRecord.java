package com.carddemo.programs.copaus0c.copybook;

/** CVACT02Y - CARD-RECORD (RECLN 150). COPYed by COPAUS0C but not referenced. FILLER X(59) dropped. */
public record CardRecord(
        String cardNum,            // CARD-NUM            PIC X(16)
        long cardAcctId,           // CARD-ACCT-ID        PIC 9(11)
        int cardCvvCd,             // CARD-CVV-CD         PIC 9(03)
        String cardEmbossedName,   // CARD-EMBOSSED-NAME  PIC X(50)
        String cardExpiraionDate,  // CARD-EXPIRAION-DATE PIC X(10)
        String cardActiveStatus) { // CARD-ACTIVE-STATUS  PIC X(01)

    public static final int RECORD_LENGTH = 150;

    public static CardRecord parse(String line) {
        String r = String.format("%-" + RECORD_LENGTH + "s", line);
        return new CardRecord(r.substring(0, 16), Long.parseLong(r.substring(16, 27)),
                Integer.parseInt(r.substring(27, 30)), r.substring(30, 80), r.substring(80, 90),
                r.substring(90, 91));
    }
}
