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

    public int size() {
        return records.size();
    }

    @Override
    public ReadResult read(String userId) {
        SecUserData record = records.get(CobolText.picX(userId, SecUserData.KEY_LENGTH));
        return record == null ? ReadResult.notFound() : ReadResult.normal(record);
    }
}
