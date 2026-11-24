package com.carddemo.transaction;

import com.carddemo.common.domain.ReportRequest;
import com.carddemo.common.domain.TransactionCategory;
import com.carddemo.common.domain.TransactionCategoryRepository;
import com.carddemo.common.domain.TransactionType;
import com.carddemo.common.domain.TransactionTypeRepository;
import com.carddemo.common.web.KeysetPage;
import java.util.List;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/v1")
public class TransactionController {

    private final TransactionService transactions;
    private final BillPaymentService billPayments;
    private final ReportRequestService reports;
    private final TransactionTypeRepository types;
    private final TransactionCategoryRepository categories;

    public TransactionController(TransactionService transactions, BillPaymentService billPayments,
                                 ReportRequestService reports, TransactionTypeRepository types,
                                 TransactionCategoryRepository categories) {
        this.transactions = transactions;
        this.billPayments = billPayments;
        this.reports = reports;
        this.types = types;
        this.categories = categories;
    }

    /** COTRN00C (CT00). */
    @GetMapping("/transactions")
    public KeysetPage<TransactionView> list(@RequestParam(required = false) String startKey,
                                            @RequestParam(required = false) String after,
                                            @RequestParam(required = false) String before,
                                            @RequestParam(defaultValue = "1") int page) {
        return transactions.list(startKey, after, before, page);
    }

    /** COTRN01C (CT01). */
    @GetMapping("/transactions/{tranId}")
    public TransactionView view(@PathVariable String tranId) {
        return transactions.view(tranId);
    }

    /** COTRN02C (CT02). 200 + confirmationRequired until confirm=Y, then 201. */
    @PostMapping("/transactions")
    public ResponseEntity<OperationResult> add(@RequestBody AddTransactionRequest request) {
        OperationResult result = transactions.add(request);
        return ResponseEntity.status(result.confirmationRequired() ? HttpStatus.OK : HttpStatus.CREATED).body(result);
    }

    /** COBIL00C (CB00). */
    @PostMapping("/bill-payments")
    public ResponseEntity<OperationResult> pay(@RequestBody BillPaymentRequest request) {
        OperationResult result = billPayments.pay(request);
        return ResponseEntity.status(result.confirmationRequired() ? HttpStatus.OK : HttpStatus.CREATED).body(result);
    }

    /** CORPT00C (CR00). */
    @PostMapping("/reports/transactions")
    @ResponseStatus(HttpStatus.ACCEPTED)
    public ReportRequest submitReport(@RequestBody ReportRequestService.ReportCommand command) {
        return reports.submit(command);
    }

    @GetMapping("/reports/transactions/{id}")
    public ReportRequest report(@PathVariable long id) {
        return reports.get(id);
    }

    /** TRANTYPE / TRANCATG reference data (COTRTLIC in the Db2 module). */
    @GetMapping("/transaction-types")
    public List<TransactionType> transactionTypes() {
        return types.findAll(Sort.by("typeCd"));
    }

    @GetMapping("/transaction-categories")
    public List<TransactionCategory> transactionCategories() {
        return categories.findAll(Sort.by("typeCd", "catCd"));
    }
}
