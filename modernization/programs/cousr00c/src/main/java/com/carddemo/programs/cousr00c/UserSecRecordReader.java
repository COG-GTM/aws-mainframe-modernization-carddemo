package com.carddemo.programs.cousr00c;

import com.carddemo.programs.cousr00c.copybook.SecUserData;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/** Reads fixed-width 80-byte SEC-USER-DATA records (e.g. the AWS.M2.CARDDEMO.USRSEC.PS unload). */
public final class UserSecRecordReader {

    private UserSecRecordReader() {
    }

    public static List<SecUserData> read(Path file, Charset charset) throws IOException {
        byte[] bytes = Files.readAllBytes(file);
        if (bytes.length % SecUserData.RECORD_LENGTH != 0) {
            throw new IOException("Not a whole number of " + SecUserData.RECORD_LENGTH + "-byte records: "
                    + bytes.length);
        }
        List<SecUserData> out = new ArrayList<>();
        for (int off = 0; off < bytes.length; off += SecUserData.RECORD_LENGTH) {
            out.add(SecUserData.fromFixedWidth(new String(bytes, off, SecUserData.RECORD_LENGTH, charset)));
        }
        return out;
    }
}
