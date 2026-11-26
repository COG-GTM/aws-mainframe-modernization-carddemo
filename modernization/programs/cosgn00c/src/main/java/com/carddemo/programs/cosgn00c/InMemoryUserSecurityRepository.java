package com.carddemo.programs.cosgn00c;

import com.carddemo.programs.cosgn00c.copybook.CobolText;
import com.carddemo.programs.cosgn00c.copybook.SecUserData;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Map;
import java.util.TreeMap;

/** In-memory USRSEC KSDS keyed on SEC-USR-ID (8 bytes), loadable from 80-byte fixed-width records. */
public final class InMemoryUserSecurityRepository implements UserSecurityRepository {

    private final Map<String, SecUserData> records = new TreeMap<>();

    public InMemoryUserSecurityRepository put(SecUserData record) {
        records.put(record.usrId(), record);
        return this;
    }

    /** Loads every non-blank line as one SEC-USER-DATA record (the DUSRSECJ / REPRO input format). */
    public static InMemoryUserSecurityRepository load(Reader reader) throws IOException {
        InMemoryUserSecurityRepository repo = new InMemoryUserSecurityRepository();
        BufferedReader br = new BufferedReader(reader);
        String line;
        while ((line = br.readLine()) != null) {
            if (!line.isBlank()) {
                repo.put(SecUserData.fromRecord(line));
            }
        }
        return repo;
    }

    /**
     * Loads an undelimited RECFM=FB stream (e.g. USRSEC.PS) as consecutive 80-character records. No
     * character is treated as a record separator, so CR/LF bytes inside a record keep their position.
     * A trailing record shorter than 80 bytes is rejected rather than space padded.
     */
    public static InMemoryUserSecurityRepository loadFixedBlock(Reader reader) throws IOException {
        InMemoryUserSecurityRepository repo = new InMemoryUserSecurityRepository();
        char[] buf = new char[SecUserData.LENGTH];
        int filled;
        while ((filled = reader.read(buf, 0, buf.length)) > 0) {
            while (filled < buf.length) {
                int n = reader.read(buf, filled, buf.length - filled);
                if (n < 0) {
                    break;
                }
                filled += n;
            }
            if (filled < buf.length) {
                throw new IOException("RECFM=FB stream ends with a short record of " + filled
                        + " bytes; expected " + SecUserData.LENGTH);
            }
            String rec = new String(buf, 0, filled);
            if (!rec.isBlank()) {
                repo.put(SecUserData.fromRecord(rec));
            }
        }
        return repo;
    }

    public int size() {
        return records.size();
    }

    @Override
    public ReadResult read(String userId) {
        SecUserData record = records.get(CobolText.picX(userId, SecUserData.KEY_LENGTH));
        return record == null ? ReadResult.notFound() : ReadResult.normal(record);
    }
}
