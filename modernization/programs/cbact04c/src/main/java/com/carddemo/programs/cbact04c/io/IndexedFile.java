package com.carddemo.programs.cbact04c.io;

/** ORGANIZATION INDEXED, ACCESS RANDOM (VSAM KSDS) file. Methods return FILE STATUS. */
public interface IndexedFile<K, T> {

    String open(OpenMode mode);

    ReadResult<T> read(K key);

    String rewrite(T record);

    String close();
}
