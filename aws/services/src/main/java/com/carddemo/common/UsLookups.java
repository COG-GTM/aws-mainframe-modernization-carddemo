package com.carddemo.common;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.Set;
import java.util.stream.Collectors;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Component;

/**
 * Lookup tables of copybook CSLKPCDY: North America general purpose area codes, US state codes and valid
 * state + first-two-ZIP-digit combinations.
 */
@Component
public class UsLookups {

    private final Set<String> areaCodes = load("lookup/na-general-purpose-area-codes.txt");
    private final Set<String> stateCodes = load("lookup/us-state-codes.txt");
    private final Set<String> stateZipCombos = load("lookup/us-state-zip2-combos.txt");

    public boolean isValidGeneralPurposeAreaCode(String code) {
        return areaCodes.contains(code);
    }

    public boolean isValidStateCode(String state) {
        return stateCodes.contains(state);
    }

    public boolean isValidStateZip(String state, String zip) {
        return zip != null && zip.length() >= 2 && stateZipCombos.contains(state + zip.substring(0, 2));
    }

    private static Set<String> load(String path) {
        try (InputStream in = new ClassPathResource(path).getInputStream();
                BufferedReader reader = new BufferedReader(new InputStreamReader(in, StandardCharsets.US_ASCII))) {
            return reader.lines().map(String::strip).filter(s -> !s.isEmpty()).collect(Collectors.toUnmodifiableSet());
        } catch (IOException ex) {
            throw new UncheckedIOException("Cannot load lookup table " + path, ex);
        }
    }
}
