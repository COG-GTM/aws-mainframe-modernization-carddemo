package com.carddemo.trantype;

import com.carddemo.common.PageResponse;
import com.carddemo.trantype.TransactionTypeService.CreateRequest;
import com.carddemo.trantype.TransactionTypeService.TransactionCategory;
import com.carddemo.trantype.TransactionTypeService.TransactionType;
import com.carddemo.trantype.TransactionTypeService.UpdateRequest;
import java.net.URI;
import java.util.List;
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
@RequestMapping("/api/v1/transaction-types")
public class TransactionTypeController {

    private final TransactionTypeService service;

    public TransactionTypeController(TransactionTypeService service) {
        this.service = service;
    }

    @GetMapping
    public PageResponse<TransactionType> list(@RequestParam(required = false) String typeCd,
            @RequestParam(required = false) String description, @RequestParam(required = false) String startKey,
            @RequestParam(required = false) String direction, @RequestParam(required = false) Integer pageSize) {
        return service.list(typeCd, description, startKey, direction, pageSize);
    }

    @GetMapping("/{typeCd}")
    public TransactionType get(@PathVariable String typeCd) {
        return service.get(typeCd);
    }

    @GetMapping("/{typeCd}/categories")
    public List<TransactionCategory> categories(@PathVariable String typeCd) {
        return service.categories(typeCd);
    }

    @PostMapping
    public ResponseEntity<TransactionType> create(@RequestBody(required = false) CreateRequest request) {
        TransactionType created = service.create(request);
        return ResponseEntity.created(URI.create("/api/v1/transaction-types/" + created.typeCd())).body(created);
    }

    @PutMapping("/{typeCd}")
    public TransactionType update(@PathVariable String typeCd, @RequestBody(required = false) UpdateRequest request) {
        return service.update(typeCd, request);
    }

    @DeleteMapping("/{typeCd}")
    public ResponseEntity<Void> delete(@PathVariable String typeCd, @RequestParam(required = false) Long version) {
        service.delete(typeCd, version);
        return ResponseEntity.noContent().build();
    }
}
