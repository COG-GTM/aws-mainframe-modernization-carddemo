package com.carddemo.batch.report;

import com.carddemo.batch.BatchOutputFile;
import com.carddemo.common.codec.RecordEncoding;
import java.util.ArrayList;
import java.util.List;

/** A TRANREPT generation: its catalogue row, the file bytes as written and their encoding. */
public record ReportFile(BatchOutputFile catalog, byte[] content, RecordEncoding encoding) {

    /** Report lines (LRECL = file size / record count), trailing spaces removed. */
    public List<String> lines() {
        List<String> lines = new ArrayList<>();
        long count = catalog.getRecordCount();
        if (count <= 0 || content.length == 0) {
            return lines;
        }
        if (encoding != RecordEncoding.EBCDIC) {
            for (String line : new String(content, encoding.charset()).split("\r?\n", -1)) {
                if (lines.size() < count) {
                    lines.add(line.stripTrailing());
                }
            }
            return lines;
        }
        int lrecl = (int) (content.length / count);
        for (int offset = 0; offset + lrecl <= content.length; offset += lrecl) {
            lines.add(encoding.decode(content, offset, lrecl).stripTrailing());
        }
        return lines;
    }

    public String fileName() {
        String path = catalog.getFilePath();
        int slash = Math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'));
        return path.substring(slash + 1);
    }
}
