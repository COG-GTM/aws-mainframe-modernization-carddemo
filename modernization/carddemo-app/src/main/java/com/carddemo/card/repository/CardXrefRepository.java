package com.carddemo.card.repository;

import com.carddemo.card.domain.CardXref;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CardXrefRepository extends JpaRepository<CardXref, String> {
}
