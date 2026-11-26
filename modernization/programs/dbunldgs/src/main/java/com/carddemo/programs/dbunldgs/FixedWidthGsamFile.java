package com.carddemo.programs.dbunldgs;

import com.carddemo.programs.dbunldgs.copybook.ImsFunctionCodes;
import com.carddemo.programs.dbunldgs.copybook.PcbMask;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * GSAM output dataset backed by a RECFM=F file: each ISRT writes exactly {@code recordLength}
 * bytes (PASFLDBD RECORD=(100), PADFLDBD RECORD=(200)). I/O failures set status 'AO'.
 */
public final class FixedWidthGsamFile implements DliCallHandler, AutoCloseable {

    private final int recordLength;
    private final OutputStream out;

    public FixedWidthGsamFile(Path path, int recordLength) {
        this.recordLength = recordLength;
        try {
            this.out = new BufferedOutputStream(Files.newOutputStream(path));
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public void cbltdli(String function, PcbMask pcb, byte[] ioArea, String ssa) {
        if (!ImsFunctionCodes.FUNC_ISRT.equals(function)) {
            pcb.setPcbStatus("AD");
            return;
        }
        byte[] rec = new byte[recordLength];
        System.arraycopy(ioArea, 0, rec, 0, Math.min(recordLength, ioArea.length));
        try {
            out.write(rec);
            pcb.setPcbStatus("  ");
        } catch (IOException e) {
            pcb.setPcbStatus("AO");
        }
    }

    @Override
    public void close() throws IOException {
        out.close();
    }
}
