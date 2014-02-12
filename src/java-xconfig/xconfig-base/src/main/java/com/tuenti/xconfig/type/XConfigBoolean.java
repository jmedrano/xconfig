/*
 * XConfigBoolean.java
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
 * XConfigBoolean class.
 */
public class XConfigBoolean implements XConfigValue {

    private Boolean value = null;

    public XConfigBoolean(boolean value) {
        this.value = value;
    }

    @Override
    public Boolean getAsBoolean() {
        return this.value;
    }

    @Override
    public String getAsString() throws XConfigWrongTypeCastingException {
	    throw new XConfigWrongTypeCastingException();
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
    public Map<String, XConfigValue> getAsMap() throws XConfigWrongTypeCastingException {
        throw new XConfigWrongTypeCastingException();
    }

    @Override
    public List<XConfigValue> getAsList() throws XConfigWrongTypeCastingException {
        throw new XConfigWrongTypeCastingException();
    }

    @Override
    public XConfigValueType getType() {
        return XConfigValueType.BOOLEAN;
    }

	@Override
	public String toString() {
		return "XConfigBoolean [value=" + value + "]";
	}
}
