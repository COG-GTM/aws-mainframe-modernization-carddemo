package com.carddemo.common.copybook;

import com.carddemo.runtime.cobol.Ebcdic;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.vsam.FlatFiles;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.util.List;

/**
 * USRSEC seed records, decoded from the original EBCDIC dataset {@code app/data/EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS}
 * (CP037, RECFM=FB, LRECL=80; the same 10 records DUSRSECJ.jcl loads). app/data/ASCII has no USRSEC file.
 */
public final class UserSecuritySeed {

    public static final String RESOURCE = "carddemo/data/EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS";
    /** CSD: DEFINE FILE(USRSEC) DSNAME(AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS). */
    public static final String FILE_NAME = "USRSEC";

    private UserSecuritySeed() {
    }

    public static List<String> records() {
        try (InputStream in = UserSecuritySeed.class.getClassLoader().getResourceAsStream(RESOURCE)) {
            if (in == null) {
                throw new IllegalStateException("Missing classpath resource " + RESOURCE);
            }
            return FlatFiles.readFixed(Ebcdic.decode(in.readAllBytes()).getBytes(PicX.CHARSET),
                    UserSecurityRecord.LENGTH);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
