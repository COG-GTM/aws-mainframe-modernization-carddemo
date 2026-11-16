/**
 * Reads and writes the CardDemo fixed-width files described by the copybooks in {@code app/cpy}
 * (zoned decimal, COMP-3, binary, ASCII and EBCDIC cp037).
 *
 * <p>{@link com.carddemo.legacyio.copybook.Copybook} parses a copybook (packaged from {@code app/cpy} at build time),
 * {@link com.carddemo.legacyio.RecordLayout} flattens it to byte positions, {@link com.carddemo.legacyio.RecordCodec}
 * converts record bytes to field values and back, and {@link com.carddemo.legacyio.LegacyFile} reads and writes
 * whole files. {@link com.carddemo.legacyio.CardDemoFile} names the datasets and their copybooks. Decoding then
 * encoding a file gives back the same bytes.
 */
package com.carddemo.legacyio;
