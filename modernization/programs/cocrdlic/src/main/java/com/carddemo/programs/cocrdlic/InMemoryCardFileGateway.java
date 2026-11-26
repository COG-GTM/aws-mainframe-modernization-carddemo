package com.carddemo.programs.cocrdlic;

import com.carddemo.programs.cocrdlic.copybook.CardRecord;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.List;
import java.util.TreeMap;

/**
 * In-memory KSDS keyed by CARD-NUM with CICS browse semantics:
 * STARTBR GTEQ positions on the first key &gt;= RIDFLD (NOTFND if none); READNEXT returns records in key order
 * then ENDFILE; the first READPREV after STARTBR returns the record only if the start key matched exactly
 * (otherwise NOTFND), later READPREVs walk backwards then ENDFILE. Commands without an active browse give INVREQ.
 * Keys compare by character value (digits and spaces collate the same in ASCII and EBCDIC).
 */
public class InMemoryCardFileGateway implements CardFileGateway {
    public static final String CARD_FILE = "CARDDAT ";

    private final TreeMap<String, CardRecord> records = new TreeMap<>();
    private final Deque<CardFileGateway.ReadResult> injectedReadResults = new ArrayDeque<>();
    private final List<String> commandLog = new ArrayList<>();
    private List<CardRecord> browse;
    private int position;
    private boolean exactStart;
    private boolean firstAfterStart;

    public InMemoryCardFileGateway() {
    }

    public InMemoryCardFileGateway(Collection<CardRecord> cards) {
        cards.forEach(this::put);
    }

    /** Load a fixed-width 150-byte CVACT02Y extract (e.g. app/data/ASCII/carddata.txt). */
    public static InMemoryCardFileGateway fromFixedWidthFile(Path path) throws IOException {
        InMemoryCardFileGateway g = new InMemoryCardFileGateway();
        for (String line : Files.readAllLines(path, StandardCharsets.US_ASCII)) {
            if (!line.isBlank()) {
                g.put(CardRecord.parse(line));
            }
        }
        return g;
    }

    public final void put(CardRecord card) {
        records.put(card.cardNum(), card);
    }

    public int size() {
        return records.size();
    }

    public List<CardRecord> allInKeyOrder() {
        return new ArrayList<>(records.values());
    }

    /** Make the next READNEXT/READPREV return this result instead of reading the file (fault injection). */
    public void injectNextRead(int resp, int resp2) {
        injectedReadResults.add(new CardFileGateway.ReadResult(resp, resp2, null));
    }

    public List<String> commandLog() {
        return commandLog;
    }

    @Override
    public int startBrowseGteq(String file, String key) {
        commandLog.add("STARTBR " + file.strip() + " GTEQ '" + key + "'");
        List<CardRecord> all = new ArrayList<>(records.values());
        int p = 0;
        while (p < all.size() && all.get(p).cardNum().compareTo(key) < 0) {
            p++;
        }
        if (p == all.size()) {
            browse = null;
            return CicsResp.NOTFND;
        }
        browse = all;
        position = p;
        exactStart = all.get(p).cardNum().equals(key);
        firstAfterStart = true;
        return CicsResp.NORMAL;
    }

    @Override
    public CardFileGateway.ReadResult readNext(String file) {
        commandLog.add("READNEXT " + file.strip());
        if (!injectedReadResults.isEmpty()) {
            return injectedReadResults.poll();
        }
        if (browse == null) {
            return new CardFileGateway.ReadResult(CicsResp.INVREQ, 0, null);
        }
        firstAfterStart = false;
        if (position >= browse.size()) {
            return new CardFileGateway.ReadResult(CicsResp.ENDFILE, 0, null);
        }
        return new CardFileGateway.ReadResult(CicsResp.NORMAL, 0, browse.get(position++));
    }

    @Override
    public CardFileGateway.ReadResult readPrev(String file) {
        commandLog.add("READPREV " + file.strip());
        if (!injectedReadResults.isEmpty()) {
            return injectedReadResults.poll();
        }
        if (browse == null) {
            return new CardFileGateway.ReadResult(CicsResp.INVREQ, 0, null);
        }
        if (firstAfterStart) {
            firstAfterStart = false;
            if (!exactStart) {
                return new CardFileGateway.ReadResult(CicsResp.NOTFND, 0, null);
            }
            return new CardFileGateway.ReadResult(CicsResp.NORMAL, 0, browse.get(position--));
        }
        if (position < 0) {
            return new CardFileGateway.ReadResult(CicsResp.ENDFILE, 0, null);
        }
        return new CardFileGateway.ReadResult(CicsResp.NORMAL, 0, browse.get(position--));
    }

    @Override
    public int endBrowse(String file) {
        commandLog.add("ENDBR " + file.strip());
        if (browse == null) {
            return CicsResp.INVREQ;
        }
        browse = null;
        return CicsResp.NORMAL;
    }
}
