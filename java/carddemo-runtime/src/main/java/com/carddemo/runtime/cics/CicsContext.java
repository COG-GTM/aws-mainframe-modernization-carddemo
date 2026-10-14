package com.carddemo.runtime.cics;

import com.carddemo.runtime.bms.BmsScreen;
import com.carddemo.runtime.bms.SymbolicMap;
import com.carddemo.runtime.cobol.CobolIntrinsics;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.vsam.FileControl;
import com.carddemo.runtime.vsam.FileResult;
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
    private final InstalledPrograms installedPrograms;
    private final List<TerminalOutput> outputs = new ArrayList<>();

    private CicsContext(Builder b) {
        this.commarea = b.commarea == null ? new byte[0] : b.commarea.clone();
        this.eib = new Eib(PicX.of(b.transid, 4), PicX.of(b.termid, 4), b.aid, commarea.length);
        this.terminalInput = b.terminalInput == null ? null : Collections.unmodifiableMap(new LinkedHashMap<>(b.terminalInput));
        this.fileControl = b.fileControl == null ? new FileControl() : b.fileControl;
        this.systemInfo = b.systemInfo == null ? SystemInfo.defaults() : b.systemInfo;
        this.clock = b.clock == null ? Clock.systemDefaultZone() : b.clock;
        this.installedPrograms = b.installedPrograms == null ? InstalledPrograms.none() : b.installedPrograms;
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

    /** Records the RESP/RESP2 of a file command executed through {@link #files()} in the EIB. */
    public FileResult track(FileResult result) {
        setResp(result.resp(), result.resp2());
        return result;
    }

    // ----------------------------------------------------------------- system programming

    /**
     * EXEC CICS INQUIRE PROGRAM(program) NOHANDLE: NORMAL when the program is installed in the region,
     * PGMIDERR otherwise; EIBRESP is set either way.
     */
    public CicsResp inquireProgram(String program) {
        return installedPrograms.contains(program) ? setResp(CicsResp.NORMAL, 0) : setResp(CicsResp.PGMIDERR, 0);
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
        private InstalledPrograms installedPrograms;

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

        /** Programs INQUIRE PROGRAM finds (default: none). */
        public Builder installedPrograms(InstalledPrograms value) {
            this.installedPrograms = value;
            return this;
        }

        public CicsContext build() {
            return new CicsContext(this);
        }
    }
}
