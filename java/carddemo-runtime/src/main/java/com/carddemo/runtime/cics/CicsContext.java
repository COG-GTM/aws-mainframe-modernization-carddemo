package com.carddemo.runtime.cics;

import com.carddemo.runtime.bms.BmsScreen;
import com.carddemo.runtime.bms.SymbolicMap;
import com.carddemo.runtime.cobol.CobolIntrinsics;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.vsam.FileControl;
import com.carddemo.runtime.vsam.FileResult;
import com.carddemo.runtime.vsam.KsdsFile;
import java.time.Clock;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * The CICS API surface available to one task: EIB, the incoming commarea, terminal I/O (BMS), file control,
 * ASSIGN, the clock, and the program-control commands.
 */
public final class CicsContext {

    /** Options of EXEC CICS SEND MAP. */
    public enum SendMapOption {
        ERASE, CURSOR, MAPONLY, DATAONLY, FREEKB, ALARM
    }

    private final Eib eib;
    private final byte[] commarea;
    private final Map<String, String> terminalInput;
    private final FileControl fileControl;
    private final SystemInfo systemInfo;
    private final Clock clock;
    private final List<TerminalOutput> outputs = new ArrayList<>();

    private CicsContext(Builder b) {
        this.commarea = b.commarea == null ? new byte[0] : b.commarea.clone();
        this.eib = new Eib(PicX.of(b.transid, 4), PicX.of(b.termid, 4), b.aid, commarea.length);
        this.terminalInput = b.terminalInput == null ? null : Collections.unmodifiableMap(new LinkedHashMap<>(b.terminalInput));
        this.fileControl = b.fileControl == null ? new FileControl() : b.fileControl;
        this.systemInfo = b.systemInfo == null ? SystemInfo.defaults() : b.systemInfo;
        this.clock = b.clock == null ? Clock.systemDefaultZone() : b.clock;
    }

    public static Builder builder() {
        return new Builder();
    }

    public Eib eib() {
        return eib;
    }

    /** DFHCOMMAREA (copy); length is {@code EIBCALEN}. */
    public byte[] commarea() {
        return commarea.clone();
    }

    public Clock clock() {
        return clock;
    }

    /** {@code FUNCTION CURRENT-DATE} using this task's clock. */
    public String currentDate() {
        return CobolIntrinsics.currentDate(clock);
    }

    // ----------------------------------------------------------------- terminal control / BMS

    /**
     * EXEC CICS RECEIVE MAP INTO(map). Applies the terminal input to the symbolic map (see
     * {@link SymbolicMap#receive}); returns MAPFAIL (map unchanged) when no field data was received.
     */
    public CicsResp receiveMap(SymbolicMap map) {
        if (terminalInput == null || terminalInput.isEmpty()) {
            return setResp(CicsResp.MAPFAIL, 0);
        }
        map.receive(terminalInput);
        return setResp(CicsResp.NORMAL, 0);
    }

    /** EXEC CICS SEND MAP FROM(map) with the given options. */
    public CicsResp sendMap(SymbolicMap map, SendMapOption... options) {
        Set<SendMapOption> opts = options.length == 0 ? EnumSet.noneOf(SendMapOption.class) : EnumSet.of(options[0], options);
        BmsScreen screen = map.render(opts.contains(SendMapOption.ERASE), opts.contains(SendMapOption.CURSOR),
                opts.contains(SendMapOption.MAPONLY), opts.contains(SendMapOption.DATAONLY));
        outputs.add(new TerminalOutput.MapOutput(screen));
        return setResp(CicsResp.NORMAL, 0);
    }

    /** EXEC CICS SEND TEXT FROM(text) LENGTH(length) [ERASE] [FREEKB]. */
    public CicsResp sendText(String text, int length, boolean erase, boolean freekb) {
        outputs.add(new TerminalOutput.TextOutput(PicX.of(text, length), erase, freekb));
        return setResp(CicsResp.NORMAL, 0);
    }

    public List<TerminalOutput> terminalOutputs() {
        return List.copyOf(outputs);
    }

    // ----------------------------------------------------------------- ASSIGN

    public String assignApplid() {
        return PicX.of(systemInfo.applid(), 8);
    }

    public String assignSysid() {
        return PicX.of(systemInfo.sysid(), 4);
    }

    // ----------------------------------------------------------------- file control

    public FileControl files() {
        return fileControl;
    }

    /** EXEC CICS READ DATASET(name) INTO(..) LENGTH(intoLength) RIDFLD(key) KEYLENGTH(len(key)). */
    public FileResult read(String dataset, String key, int intoLength) {
        return track(fileControl.read(dataset, key, intoLength));
    }

    /**
     * EXEC CICS READ DATASET(name) UPDATE RIDFLD(key): locks the record against other tasks' READ UPDATE until
     * SYNCPOINT / task end (RECORDBUSY when the lock wait times out), then holds it for REWRITE.
     */
    public FileResult readForUpdate(String dataset, String key) {
        if (!fileControl.recordLocks().acquire(dataset, key, this)) {
            return track(FileResult.of(CicsResp.RECORDBUSY));
        }
        return track(fileControl.readForUpdate(dataset, key));
    }

    /** EXEC CICS READ DATASET(path) RIDFLD(alternate key) through an alternate-index PATH. */
    public FileResult readPath(String dataset, String altKey) {
        return track(fileControl.readPath(dataset, altKey));
    }

    /** EXEC CICS REWRITE FILE(name) FROM(record): logged so SYNCPOINT ROLLBACK / ABEND can back it out. */
    public FileResult rewrite(String dataset, String record) {
        java.util.Optional<KsdsFile> file = fileControl.ksds(dataset);
        byte[] before = file.flatMap(f -> f.store().get(keyOf(f, record))).orElse(null);
        FileResult r = track(fileControl.rewrite(dataset, record));
        if (r.isNormal() && file.isPresent()) {
            unitOfWork.add(new UndoEntry(file.get(), keyOf(file.get(), record), before));
        }
        return r;
    }

    /** EXEC CICS WRITE FILE(name) FROM(record): logged for backout. */
    public FileResult write(String dataset, String record) {
        java.util.Optional<KsdsFile> file = fileControl.ksds(dataset);
        FileResult r = track(fileControl.write(dataset, record));
        if (r.isNormal() && file.isPresent()) {
            unitOfWork.add(new UndoEntry(file.get(), keyOf(file.get(), record), null));
        }
        return r;
    }

    /** EXEC CICS SYNCPOINT: commits this task's file changes and releases its READ UPDATE holds. */
    public void syncpoint() {
        unitOfWork.clear();
        fileControl.ksdsFiles().forEach(KsdsFile::unlock);
        fileControl.recordLocks().releaseAll(this);
    }

    /** Task end: releases this task's record locks (CICS implicit syncpoint / backout already applied). */
    public void releaseRecordLocks() {
        fileControl.recordLocks().releaseAll(this);
    }

    /**
     * EXEC CICS SYNCPOINT ROLLBACK: backs out the changes made through {@link #rewrite}/{@link #write} since
     * the last syncpoint (most recent first) and releases READ UPDATE holds. Also run on ABEND.
     */
    public void syncpointRollback() {
        for (int i = unitOfWork.size() - 1; i >= 0; i--) {
            UndoEntry e = unitOfWork.get(i);
            if (e.before() == null) {
                e.file().store().delete(e.key());
            } else {
                e.file().store().update(e.key(), e.before());
            }
        }
        syncpoint();
    }

    /** Uncommitted changes of this task (rewrite/write since the last syncpoint). */
    public int uncommittedChanges() {
        return unitOfWork.size();
    }

    private record UndoEntry(KsdsFile file, byte[] key, byte[] before) {
    }

    private final List<UndoEntry> unitOfWork = new ArrayList<>();

    private static byte[] keyOf(KsdsFile f, String record) {
        String r = PicX.of(record, Math.max(record.length(), f.keyOffset() + f.keyLength()));
        return PicX.toBytes(r.substring(f.keyOffset(), f.keyOffset() + f.keyLength()));
    }

    /** Records the RESP/RESP2 of a file command executed through {@link #files()} in the EIB. */
    public FileResult track(FileResult result) {
        setResp(result.resp(), result.resp2());
        return result;
    }

    // ----------------------------------------------------------------- program control (non-returning)

    /** EXEC CICS RETURN (no TRANSID): ends the task and the pseudo-conversation. */
    public void returnToCics() {
        throw new CicsControlTransfer(CicsOutcome.returnToCics());
    }

    /** EXEC CICS RETURN TRANSID(transid) COMMAREA(commarea) LENGTH(LENGTH OF commarea). */
    public void returnTransid(String transid, CobolRecord commareaRecord) {
        throw new CicsControlTransfer(CicsOutcome.returnTransid(PicX.of(transid, 4),
                commareaRecord == null ? null : commareaRecord.toBytes()));
    }

    /** EXEC CICS XCTL PROGRAM(program) COMMAREA(commarea). */
    public void xctl(String program, CobolRecord commareaRecord) {
        throw new CicsControlTransfer(CicsOutcome.xctl(PicX.of(program, 8),
                commareaRecord == null ? null : commareaRecord.toBytes()));
    }

    /** EXEC CICS ABEND ABCODE(code). */
    public void abend(String code) {
        throw new CicsControlTransfer(CicsOutcome.abend(code));
    }

    private CicsResp setResp(CicsResp resp, int resp2) {
        eib.setResp(resp.code(), resp2);
        return resp;
    }

    /** Builder for a task context. */
    public static final class Builder {
        private String transid = "";
        private String termid = "T001";
        private AidKey aid = AidKey.ENTER;
        private byte[] commarea;
        private Map<String, String> terminalInput;
        private FileControl fileControl;
        private SystemInfo systemInfo;
        private Clock clock;

        public Builder transid(String value) {
            this.transid = Objects.requireNonNull(value);
            return this;
        }

        public Builder termid(String value) {
            this.termid = Objects.requireNonNull(value);
            return this;
        }

        public Builder aid(AidKey value) {
            this.aid = Objects.requireNonNull(value);
            return this;
        }

        /** Incoming commarea; null or empty means EIBCALEN = 0. */
        public Builder commarea(byte[] value) {
            this.commarea = value;
            return this;
        }

        /** Field values the terminal transmitted (field name -> typed text); null means no input. */
        public Builder terminalInput(Map<String, String> value) {
            this.terminalInput = value;
            return this;
        }

        public Builder fileControl(FileControl value) {
            this.fileControl = value;
            return this;
        }

        public Builder systemInfo(SystemInfo value) {
            this.systemInfo = value;
            return this;
        }

        public Builder clock(Clock value) {
            this.clock = value;
            return this;
        }

        public CicsContext build() {
            return new CicsContext(this);
        }
    }
}
