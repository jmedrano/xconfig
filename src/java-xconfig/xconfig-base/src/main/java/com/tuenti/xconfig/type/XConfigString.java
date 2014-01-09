/*
 * XConfigString.java
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
 * XConfigString class.
 */
public class XConfigString implements XConfigValue {

    private String value;

    public XConfigString(String value) {
        this.value = value;
    }

    @Override
    public String getAsString() {
        return this.value;
    }

    @Override
    public Integer getAsInteger() throws XConfigWrongTypeCastingException {
        throw new XConfigWrongTypeCastingException();
    }

    @Override
    public Float getAsFloat() throws XConfigWrongTypeCastingException {
        throw new XConfigWrongTypeCastingException();
    }

    @Override
    public Boolean getAsBoolean() throws XConfigWrongTypeCastingException {
        throw new XConfigWrongTypeCastingException();
    }

    @Override
    public Map<String, XConfigValue> getAsMap() throws XConfigWrongTypeCastingException {
        throw new XConfigWrongTypeCastingException();
    }

    @Override
    public List<XConfigValue> getAsList() throws XConfigWrongTypeCastingException {
        throw new XConfigWrongTypeCastingException();
    }

    @Override
    public XConfigValueType getType() {
        return XConfigValueType.STRING;
    }

	@Override
	public String toString() {
		return "XConfigString [value=" + value + "]";
	}
}
