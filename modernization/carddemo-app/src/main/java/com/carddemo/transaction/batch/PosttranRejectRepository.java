package com.carddemo.transaction.batch;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface PosttranRejectRepository extends JpaRepository<PosttranReject, PosttranReject.Key> {

    List<PosttranReject> findByJobInstanceIdOrderByInputSeq(long jobInstanceId);
}
