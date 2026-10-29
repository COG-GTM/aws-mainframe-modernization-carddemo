package com.carddemo.transactiontype.api;

import com.carddemo.transactiontype.domain.MaintenanceResult;
import com.carddemo.transactiontype.domain.PageDirection;
import com.carddemo.transactiontype.domain.TransactionType;
import com.carddemo.transactiontype.domain.TransactionTypeCategory;
import com.carddemo.transactiontype.domain.TransactionTypePage;
import com.carddemo.transactiontype.domain.TransactionTypeService;
import com.carddemo.transactiontype.domain.batch.BatchMaintenanceService;
import com.carddemo.transactiontype.domain.batch.BatchResult;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.util.List;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1")
@Tag(name = "Transaction types", description = "COTRTLIC / COTRTUPC / COBTUPDT / TRANEXTR on CARDDEMO.TRANSACTION_TYPE")
public class TransactionTypeController {

    private final TransactionTypeService service;
    private final BatchMaintenanceService batch;

    public TransactionTypeController(TransactionTypeService service, BatchMaintenanceService batch) {
        this.service = service;
        this.batch = batch;
    }

    @GetMapping("/transaction-types")
    @Operation(summary = "List (COTRTLIC)", description = "7 rows per page ordered by code. FIRST = Enter, "
            + "NEXT = PF8 with startKey=nextStartKey, PREVIOUS = PF7 with startKey=firstKey.")
    public TransactionTypePage list(
            @Parameter(description = "Exact type code filter, 2 digits") @RequestParam(required = false) String typeCode,
            @Parameter(description = "Contains filter on description (case-sensitive LIKE)")
            @RequestParam(required = false) String description,
            @RequestParam(required = false) String startKey,
            @RequestParam(defaultValue = "FIRST") PageDirection direction) {
        return service.list(typeCode, description, startKey, direction);
    }

    @GetMapping("/transaction-types/{code}")
    @Operation(summary = "Get one (COTRTUPC Enter on a key)")
    public TransactionType get(@PathVariable String code) {
        return service.get(code);
    }

    @PostMapping("/transaction-types")
    @Operation(summary = "Create (COTRTUPC PF5 on a new key)")
    public ResponseEntity<MaintenanceResult> create(@RequestBody TransactionTypeRequest request) {
        return ResponseEntity.status(HttpStatus.CREATED).body(service.create(request.code(), request.description()));
    }

    @PutMapping("/transaction-types/{code}")
    @Operation(summary = "Update description (COTRTLIC 'U' + F10 / COTRTUPC PF5)",
            description = "createIfMissing=true reproduces COTRTUPC 9600-WRITE-PROCESSING: UPDATE, then INSERT on SQLCODE +100.")
    public ResponseEntity<MaintenanceResult> update(@PathVariable String code, @RequestBody DescriptionRequest request,
                                                    @RequestParam(defaultValue = "false") boolean createIfMissing) {
        MaintenanceResult r = service.update(code, request.description(), createIfMissing);
        return ResponseEntity.status(r.created() ? HttpStatus.CREATED : HttpStatus.OK).body(r);
    }

    @DeleteMapping("/transaction-types/{code}")
    @Operation(summary = "Delete (COTRTUPC PF4 + PF4 / COTRTLIC 'D' + F10)",
            description = "Without confirm=true returns 428 with the legacy confirmation prompt.")
    public MaintenanceResult delete(@PathVariable String code, @RequestParam(defaultValue = "false") boolean confirm) {
        return service.delete(code, confirm);
    }

    @GetMapping("/transaction-types/{code}/categories")
    @Operation(summary = "Child rows in TRANSACTION_TYPE_CATEGORY (the ON DELETE RESTRICT dependants)")
    public List<TransactionTypeCategory> categories(@PathVariable String code) {
        return service.categories(code);
    }

    @PostMapping(value = "/transaction-types/batch", consumes = MediaType.TEXT_PLAIN_VALUE)
    @Operation(summary = "Batch maintenance (MNTTRDB2 / COBTUPDT)",
            description = "One 53-byte record per line: action A/U/D/* + 2-char code + 50-char description.")
    public BatchResult runBatch(@RequestBody String input) {
        return batch.run(input.lines().toList());
    }

    @GetMapping(value = "/transaction-types/extract", produces = MediaType.TEXT_PLAIN_VALUE)
    @Operation(summary = "TRANEXTR STEP40: TRANTYPE.PS 60-byte records")
    public String extractTypes() {
        return toRecords(service.extractTransactionTypes());
    }

    @GetMapping(value = "/transaction-type-categories/extract", produces = MediaType.TEXT_PLAIN_VALUE)
    @Operation(summary = "TRANEXTR STEP50: TRANCATG.PS 60-byte records")
    public String extractCategories() {
        return toRecords(service.extractCategories());
    }

    static String toRecords(List<String> records) {
        return records.isEmpty() ? "" : String.join("\n", records) + "\n";
    }
}
