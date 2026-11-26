package com.carddemo.programs.cousr00c;

import com.carddemo.programs.cousr00c.copybook.CardDemoCommarea;

/** COUSR00C's CARDDEMO-COMMAREA: COCOM01Y (160 bytes) followed by CDEMO-CU00-INFO (34 bytes). */
public class Cousr00cCommarea {

    public static final int LENGTH = CardDemoCommarea.LENGTH + Cu00Info.LENGTH;

    private final CardDemoCommarea general;
    private final Cu00Info cu00;

    public Cousr00cCommarea() {
        this(new CardDemoCommarea(), new Cu00Info());
    }

    public Cousr00cCommarea(CardDemoCommarea general, Cu00Info cu00) {
        this.general = general;
        this.cu00 = cu00;
    }

    /** MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA: space padded / truncated to 194 bytes. */
    public static Cousr00cCommarea fromFixedWidth(String raw) {
        String s = Cobol.alnum(raw, LENGTH);
        return new Cousr00cCommarea(CardDemoCommarea.fromFixedWidth(s.substring(0, CardDemoCommarea.LENGTH)),
                Cu00Info.fromFixedWidth(s.substring(CardDemoCommarea.LENGTH)));
    }

    public String toFixedWidth() {
        return general.toFixedWidth() + cu00.toFixedWidth();
    }

    public CardDemoCommarea general() {
        return general;
    }

    public Cu00Info cu00() {
        return cu00;
    }
}
