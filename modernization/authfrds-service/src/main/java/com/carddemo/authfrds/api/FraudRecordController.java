package com.carddemo.authfrds.api;

import com.carddemo.authfrds.domain.FraudFilter;
import com.carddemo.authfrds.domain.FraudRecord;
import com.carddemo.authfrds.domain.FraudRecordPage;
import com.carddemo.authfrds.domain.FraudRecordStore;
import com.carddemo.authfrds.domain.FraudRecordService;
import com.carddemo.authfrds.domain.FraudRequest;
import com.carddemo.authfrds.domain.FraudResult;
import com.carddemo.authfrds.domain.LegacyRuleException;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import java.time.LocalDateTime;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/fraud-records")
@Tag(name = "1. AUTHFRDS fraud records (COPAUS2C)")
public class FraudRecordController {

    private final FraudRecordService service;
    private final FraudRecordStore store;
    private final int pageSize;

    public FraudRecordController(FraudRecordService service, FraudRecordStore store,
                                 @Value("${carddemo.list.page-size:5}") int pageSize) {
        this.service = service;
        this.store = store;
        this.pageSize = pageSize;
    }

    @PostMapping
    @Operation(summary = "COPAUS2C: INSERT the authorization into AUTHFRDS; on SQLCODE -803 UPDATE the fraud flag",
            description = "201 ADD SUCCESS, 200 UPDT SUCCESS, 422 WS-FRD-UPDT-FAILED (SYSTEM ERROR DB2 / UPDT ERROR DB2)")
    public ResponseEntity<FraudResult> record(@Valid @RequestBody FraudRequest request) {
        FraudResult r = service.record(request);
        HttpStatus status = !r.success() ? HttpStatus.UNPROCESSABLE_ENTITY
                : "ADD".equals(r.operation()) ? HttpStatus.CREATED : HttpStatus.OK;
        return ResponseEntity.status(status).body(r);
    }

    @GetMapping
    @Operation(summary = "Inspection read (not in the legacy programs): AUTHFRDS rows in XAUTHFRD order, paged")
    public FraudRecordPage list(@RequestParam(required = false) String cardNum,
                                @RequestParam(required = false) Long acctId,
                                @RequestParam(required = false) String authFraud,
                                @RequestParam(defaultValue = "0") int page) {
        if (page < 0) {
            throw new LegacyRuleException("BR-AF-030", LegacyRuleException.Kind.VALIDATION, "page must be >= 0");
        }
        FraudFilter f = new FraudFilter(blankToNull(cardNum), acctId, blankToNull(authFraud));
        long total = store.count(f);
        return new FraudRecordPage(store.list(f, page * pageSize, pageSize), page, pageSize, total, page > 0,
                (long) (page + 1) * pageSize < total);
    }

    @GetMapping("/{cardNum}/{authTs}")
    @Operation(summary = "Inspection read of one AUTHFRDS row by primary key; 404 = SQLCODE +100")
    public FraudRecord get(@PathVariable String cardNum,
                           @PathVariable @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime authTs) {
        return store.find(cardNum, authTs).orElseThrow(() -> new LegacyRuleException("BR-AF-007",
                LegacyRuleException.Kind.NOT_FOUND, "no AUTHFRDS row for " + cardNum + " / " + authTs, 100));
    }

    private static String blankToNull(String s) {
        return s == null || s.isBlank() ? null : s;
    }
}
