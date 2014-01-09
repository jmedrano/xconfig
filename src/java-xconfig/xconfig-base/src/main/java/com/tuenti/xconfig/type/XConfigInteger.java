/*
 * XConfigInteger.java
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
 * XConfigInteger class.
 */
public class XConfigInteger implements XConfigValue {

    private Integer value = null;

    public XConfigInteger(int value) {
        this.value = value;
    }

    @Override
    public Integer getAsInteger() {
        return this.value;
    }

    @Override
    public String getAsString() {
        return Integer.toString(this.value);
    }

    @Override
    public Float getAsFloat() {
        return (float) this.value;
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
        return XConfigValueType.INTEGER;
    }

	@Override
	public String toString() {
		return "XConfigInteger [value=" + value + "]";
	}
}
