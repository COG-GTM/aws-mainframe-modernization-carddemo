package com.carddemo.batch;

import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Limit;
import org.springframework.data.jpa.repository.JpaRepository;

/** GDG generation lookups on index {@code batch_output_file_generation_ix} (ADR-0012). */
public interface BatchOutputFileRepository extends JpaRepository<BatchOutputFile, Long> {

    /** Generations of {@code gdgBase}, newest first: element 0 is {@code (0)}, element 1 is {@code (-1)}. */
    List<BatchOutputFile> findByGdgBaseOrderByBusinessDateDescJobExecutionIdDesc(String gdgBase, Limit limit);

    /** Relative generation {@code (0)}, {@code (-1)}, ... of {@code gdgBase}. */
    default Optional<BatchOutputFile> generation(String gdgBase, int relative) {
        if (relative > 0) {
            throw new IllegalArgumentException("(+" + relative + ") is a new generation, not a lookup");
        }
        List<BatchOutputFile> newest = findByGdgBaseOrderByBusinessDateDescJobExecutionIdDesc(gdgBase,
                Limit.of(1 - relative));
        return newest.size() > -relative ? Optional.of(newest.get(-relative)) : Optional.empty();
    }
}
