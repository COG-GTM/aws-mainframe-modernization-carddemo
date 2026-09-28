package com.carddemo.batch.storage;

public class ObjectNotFoundException extends RuntimeException {

    public ObjectNotFoundException(String uri) {
        super("Object not found: " + uri);
    }
}
