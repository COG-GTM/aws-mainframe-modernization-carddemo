package com.carddemo.common.domain;

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ReportRequestRepository extends JpaRepository<ReportRequest, Long> {

    List<ReportRequest> findByStatusOrderByIdAsc(String status);
}
