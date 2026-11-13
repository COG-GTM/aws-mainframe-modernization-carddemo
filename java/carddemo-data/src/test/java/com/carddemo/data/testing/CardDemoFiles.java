package com.carddemo.data.testing;

import com.carddemo.common.testing.CardDemoDataset;
import com.carddemo.data.catalog.CardDemoFile;

import java.util.Arrays;

/** Maps the sample datasets of app/data to the target files/tables. */
public final class CardDemoFiles {

    private CardDemoFiles() {
    }

    public static CardDemoFile of(CardDemoDataset dataset) {
        return Arrays.stream(CardDemoFile.values())
                .filter(f -> f.copybook().equals(dataset.copybook()))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("No target file for " + dataset));
    }
}
