package com.carddemo.transaction.repository;

import com.carddemo.transaction.domain.Card;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CardRepository extends JpaRepository<Card, String> {
}
