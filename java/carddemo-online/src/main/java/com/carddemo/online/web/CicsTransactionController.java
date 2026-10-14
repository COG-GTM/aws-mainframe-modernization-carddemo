package com.carddemo.online.web;

import com.carddemo.runtime.cics.AidKey;
import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.CicsDispatcher;
import com.carddemo.runtime.cics.CicsProgram;
import com.carddemo.runtime.cics.CicsResponse;
import com.carddemo.runtime.cics.SystemInfo;
import com.carddemo.runtime.cics.TerminalOutput;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.vsam.FileControl;
import java.time.Clock;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.server.ResponseStatusException;

/**
 * Terminal-facing REST adapter: {@code POST /api/cics/{transid}} runs one task of the program the
 * transaction maps to (CSD TRANSACTION -> PROGRAM), {@code POST /api/cics/programs/{program}} runs a
 * program directly (as after an XCTL). Every migrated {@link CicsProgram} bean is picked up automatically.
 */
@RestController
@RequestMapping("/api/cics")
public class CicsTransactionController {

    private final Map<String, CicsProgram> byTransid = new LinkedHashMap<>();
    private final Map<String, CicsProgram> byProgram = new LinkedHashMap<>();
    private final FileControl fileControl;
    private final SystemInfo systemInfo;
    private final Clock clock;

    public CicsTransactionController(List<CicsProgram> programs, FileControl fileControl, SystemInfo systemInfo,
                                     Clock clock) {
        for (CicsProgram p : programs) {
            byProgram.put(p.programName(), p);
            p.transactionId().ifPresent(t -> byTransid.put(t, p));
        }
        this.fileControl = fileControl;
        this.systemInfo = systemInfo;
        this.clock = clock;
    }

    @GetMapping("/transactions")
    public Map<String, String> transactions() {
        Map<String, String> out = new LinkedHashMap<>();
        byTransid.forEach((t, p) -> out.put(t, p.programName()));
        return out;
    }

    @PostMapping("/{transid}")
    public CicsTransactionResponse transaction(@PathVariable String transid,
                                               @RequestBody(required = false) CicsTransactionRequest request) {
        CicsProgram program = Optional.ofNullable(byTransid.get(transid.trim().toUpperCase()))
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.NOT_FOUND, "Unknown transaction " + transid));
        return run(program, transid.trim().toUpperCase(), request);
    }

    @PostMapping("/programs/{program}")
    public CicsTransactionResponse program(@PathVariable String program,
                                           @RequestBody(required = false) CicsTransactionRequest request) {
        CicsProgram p = Optional.ofNullable(byProgram.get(program.trim().toUpperCase()))
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.NOT_FOUND,
                        "Program not migrated: " + program));
        return run(p, p.transactionId().orElse(""), request);
    }

    private CicsTransactionResponse run(CicsProgram program, String transid, CicsTransactionRequest request) {
        CicsTransactionRequest req = request == null ? new CicsTransactionRequest(null, null, null, null) : request;
        AidKey aid;
        try {
            aid = req.aid() == null || req.aid().isBlank() ? AidKey.ENTER : AidKey.valueOf(req.aid().trim().toUpperCase());
        } catch (IllegalArgumentException e) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Unknown AID " + req.aid());
        }
        byte[] commarea;
        try {
            commarea = req.commarea() == null ? null : Base64.getDecoder().decode(req.commarea());
        } catch (IllegalArgumentException e) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "commarea must be base64");
        }
        CicsContext ctx = CicsContext.builder().transid(transid).termid(req.termid() == null ? "T001" : req.termid())
                .aid(aid).commarea(commarea).terminalInput(req.fields()).fileControl(fileControl)
                .systemInfo(systemInfo).clock(clock).build();
        CicsResponse r;
        try {
            r = CicsDispatcher.run(program, ctx);
        } catch (IllegalArgumentException e) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, e.getMessage());
        }
        byte[] out = r.outcome().commarea();
        Map<String, Object> decoded = out == null ? null
                : program.commareaLayout().map(l -> l.describe(CobolRecord.ofBytes(out))).orElse(null);
        TerminalOutput.MapOutput map = r.lastMap();
        TerminalOutput.TextOutput text = r.lastText();
        return new CicsTransactionResponse(r.program(), r.outcome().type().name(), r.outcome().nextTransid(),
                r.outcome().nextProgram(), out == null ? null : Base64.getEncoder().encodeToString(out), decoded,
                map == null ? null : map.screen(), map == null ? null : map.screen().render(),
                text == null ? null : text.text(), r.eib().eibresp(), r.eib().eibresp2(), r.outcome().abendCode());
    }
}
