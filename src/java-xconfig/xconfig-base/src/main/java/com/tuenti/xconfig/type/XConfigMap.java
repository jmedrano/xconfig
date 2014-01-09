/*
 * XConfigList.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig.type;

import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;

import java.util.*;

/**
 * Created by aaron on 16/12/13.
 */
public class XConfigMap implements XConfigValue {

    private Map<String, XConfigValue> values = null;

    public XConfigMap() {
        this.values = new HashMap<String, XConfigValue>();
    }

    public void add(String key, XConfigValue value) {
        this.values.put(key, value);
    }

    @Override
    public Map<String, XConfigValue> getAsMap() {
        return Collections.unmodifiableMap(values);
    }

    @Override
    public Integer getAsInteger() throws XConfigWrongTypeCastingException {
        throw new XConfigWrongTypeCastingException();
    }

    @Override
    public String getAsString() throws XConfigWrongTypeCastingException {
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
    public List<XConfigValue> getAsList() throws XConfigWrongTypeCastingException {
        throw new XConfigWrongTypeCastingException();
    }

    @Override
    public XConfigValueType getType() {
        return XConfigValueType.MAP;
    }

	@Override
	public String toString() {
		return "XConfigMap [values=" + values + "]";
	}
}
