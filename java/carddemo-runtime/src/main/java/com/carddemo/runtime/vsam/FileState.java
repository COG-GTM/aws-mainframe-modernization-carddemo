package com.carddemo.runtime.vsam;

/** CICS file status as set by CEMT SET FILE: an unusable file returns NOTOPEN or DISABLED. */
public enum FileState {
    ENABLED, DISABLED, CLOSED
}
