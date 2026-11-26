package com.carddemo.programs.cocrdslc;

import com.carddemo.programs.cocrdslc.copybook.CardRecord;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

/** In-memory KSDS keyed by CARD-NUM with a non-unique CARD-ACCT-ID alternate index (first match wins). */
public final class InMemoryCardFileGateway implements CardFileGateway {

    private final Map<String, CardRecord> byCardNum = new LinkedHashMap<>();
    private CardReadResponse forcedResponse;

    public InMemoryCardFileGateway put(CardRecord record) {
        byCardNum.put(record.cardNum(), record);
        return this;
    }

    /** Load fixed-width CARD-RECORD lines (e.g. app/data/ASCII/carddata.txt). */
    public static InMemoryCardFileGateway load(InputStream in) throws IOException {
        InMemoryCardFileGateway g = new InMemoryCardFileGateway();
        try (BufferedReader r = new BufferedReader(new InputStreamReader(in, StandardCharsets.US_ASCII))) {
            String line;
            while ((line = r.readLine()) != null) {
                if (!line.isBlank()) {
                    g.put(CardRecord.parse(line));
                }
            }
        }
        return g;
    }

    /** Make every subsequent READ return this RESP/RESP2 (simulates NOTOPEN, DISABLED, ...). */
    public InMemoryCardFileGateway failWith(int resp, int resp2) {
        forcedResponse = CardReadResponse.error(resp, resp2);
        return this;
    }

    public int size() {
        return byCardNum.size();
    }

    @Override
    public CardReadResponse readCardDat(String cardNum16) {
        if (forcedResponse != null) {
            return forcedResponse;
        }
        CardRecord rec = byCardNum.get(CobolText.pic(cardNum16, CardRecord.CARD_NUM_LEN));
        return rec == null ? CardReadResponse.notFound() : CardReadResponse.normal(rec);
    }

    @Override
    public CardReadResponse readCardAix(String acctId11) {
        if (forcedResponse != null) {
            return forcedResponse;
        }
        long acct = CobolText.zonedValue(acctId11);
        Optional<CardRecord> rec = byCardNum.values().stream()
                .filter(c -> c.cardAcctId() == acct)
                .findFirst();
        return rec.map(CardReadResponse::normal).orElseGet(CardReadResponse::notFound);
    }
}
