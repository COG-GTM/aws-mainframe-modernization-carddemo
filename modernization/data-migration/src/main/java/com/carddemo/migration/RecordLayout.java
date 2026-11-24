package com.carddemo.migration;

import java.util.List;

/** Fixed-length record layout derived from a copybook, plus the target table it loads into. */
public record RecordLayout(String copybook, String table, int recordLength, List<Field> fields) {

    public RecordLayout {
        for (Field f : fields) {
            if (f.offset() + f.length() > recordLength) {
                throw new IllegalArgumentException(copybook + "." + f.copybookName() + " exceeds record length");
            }
        }
    }
}
