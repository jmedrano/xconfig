/*
 * XConfigMap.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig.type;

import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;

import java.util.*;

/**
 * XConfigMap class.
 */
public class XConfigList implements XConfigValue {

    private List<XConfigValue> values = null;

    public XConfigList() {
        this.values = new ArrayList<XConfigValue>();
    }

    public void add(XConfigValue value) {
        this.values.add(value);
    }

    @Override
    public List<XConfigValue> getAsList() {
        return Collections.unmodifiableList(values);
    }

    @Override
    public Map<String, XConfigValue> getAsMap() throws XConfigWrongTypeCastingException {
        throw new XConfigWrongTypeCastingException();
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
    public XConfigValueType getType() {
        return XConfigValueType.LIST;
    }

	@Override
	public String toString() {
		return "XConfigList [values=" + values + "]";
	}
}