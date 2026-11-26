package com.carddemo.programs.copaua0c.copybook;

import com.carddemo.programs.copaua0c.support.Cobol;

/** CMQODV - MQOD fields used by COPAUA0C. */
public record MqObjectDescriptor(int objectType, String objectName) {

    public MqObjectDescriptor {
        objectName = Cobol.alpha(objectName, 48);
    }
}
