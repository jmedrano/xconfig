/*
 * XConfigValue.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig.type;

import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;

import java.util.List;
import java.util.Map;

/**
 * XConfigValue interface.
 */
public interface XConfigValue {
    public Integer getAsInteger() throws XConfigWrongTypeCastingException;
    public String getAsString() throws XConfigWrongTypeCastingException;
    public Float getAsFloat() throws XConfigWrongTypeCastingException;
    public Boolean getAsBoolean() throws XConfigWrongTypeCastingException;
    public Map<String, XConfigValue> getAsMap() throws XConfigWrongTypeCastingException;
    public List<XConfigValue> getAsList() throws XConfigWrongTypeCastingException;
    public XConfigValueType getType();
}
