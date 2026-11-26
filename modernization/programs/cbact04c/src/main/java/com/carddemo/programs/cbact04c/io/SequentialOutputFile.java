package com.carddemo.programs.cbact04c.io;

/** ORGANIZATION SEQUENTIAL file opened OUTPUT. Methods return FILE STATUS. */
public interface SequentialOutputFile<T> {

    String open();

    String write(T record);

    String close();
}
