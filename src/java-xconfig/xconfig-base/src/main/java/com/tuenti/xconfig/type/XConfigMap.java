/*
 * XConfigMap.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig.type;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;

/**
 * XConfigMap class.
 */
public class XConfigMap implements XConfigValue {

	private Map<String, XConfigValue> values = null;

	public XConfigMap() {
		this.values = new HashMap<String, XConfigValue>();
	}

	public XConfigMap(Map<String, XConfigValue> map) {
		this.values = new HashMap<String, XConfigValue>(map);
	}

	/*
	 * XConfigValue methods
	 */

	@Override
	public XConfigMap getAsMap() {
		return this;
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
	public XConfigList getAsList() throws XConfigWrongTypeCastingException {
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

	/*
	 * Own methods
	 */
	public void add(String key, XConfigValue value) {
		values.put(key, value);
	}

	/*
	 * Delegated methods to Map
	 */

	public int size() {
		return values.size();
	}

	public boolean isEmpty() {
		return values.isEmpty();
	}

	public boolean containsKey(String key) {
		return values.containsKey(key);
	}

	public boolean containsValue(XConfigValue value) {
		return values.containsValue(value);
	}

	public XConfigValue get(String key) throws XConfigKeyNotFoundException {
		if (!values.containsKey(key)) {
			throw new XConfigKeyNotFoundException(key);
		}
		return values.get(key);
	}

	public Set<String> keySet() {
		return values.keySet();
	}

	public Collection<XConfigValue> values() {
		return values.values();
	}

	public Set<Map.Entry<String,XConfigValue>> entrySet() {
		return values.entrySet();
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;

		XConfigMap that = (XConfigMap) o;

		if (!values.equals(that.values)) return false;

		return true;
	}

	@Override
	public int hashCode() {
		return values.hashCode();
	}

	@Override
	public Map<String, Object> getAsJavaObject() {
		Map<String, Object> javaValues = new HashMap<String, Object>();
		for (Entry<String, XConfigValue> entry : values.entrySet()) {
			javaValues.put(entry.getKey(), entry.getValue().getAsJavaObject());
		}
		return javaValues;
	}
}
