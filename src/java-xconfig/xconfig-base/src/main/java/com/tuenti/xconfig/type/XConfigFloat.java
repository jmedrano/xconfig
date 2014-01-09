/*
 * XConfigFloat.java
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
 * XConfigFloat class.
 */
public class XConfigFloat implements XConfigValue {

    private Float value = null;

    public XConfigFloat(float value) {
        this.value = value;
    }

    @Override
    public Float getAsFloat() {
        return this.value;
    }

    @Override
    public String getAsString() {
        return Float.toString(this.value);
    }

    @Override
	public String toString() {
		return "XConfigFloat [value=" + value + "]";
	}

	@Override
    public Boolean getAsBoolean() throws XConfigWrongTypeCastingException {
        throw new XConfigWrongTypeCastingException();
    }

    @Override
    public Integer getAsInteger() throws XConfigWrongTypeCastingException {
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
        return XConfigValueType.FLOAT;
    }
}
