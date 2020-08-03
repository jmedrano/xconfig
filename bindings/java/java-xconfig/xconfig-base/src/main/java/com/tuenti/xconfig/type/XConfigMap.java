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
	public Long getAsLong() throws XConfigWrongTypeCastingException {
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

	/*
	 * Additional methods to improve usability
	 */
	public XConfigMap getAsMap(String key, XConfigMap defaultValue) {
		try {
			return get(key).getAsMap();
		} catch (XConfigWrongTypeCastingException | XConfigKeyNotFoundException ignored) {
			return defaultValue;
		}
	}

	public Integer getAsInteger(String key, Integer defaultValue) {
		try {
			return get(key).getAsInteger();
		} catch (XConfigWrongTypeCastingException | XConfigKeyNotFoundException ignored) {
			return defaultValue;
		}
	}

	public String getAsString(String key, String defaultValue) {
		try {
			return get(key).getAsString();
		} catch (XConfigWrongTypeCastingException | XConfigKeyNotFoundException ignored) {
			return defaultValue;
		}
	}

	public Float getAsFloat(String key, Float defaultValue) {
		try {
			return get(key).getAsFloat();
		} catch (XConfigWrongTypeCastingException | XConfigKeyNotFoundException ignored) {
			return defaultValue;
		}
	}

	public Long getAsLong(String key, Long defaultValue) {
		try {
			return get(key).getAsLong();
		} catch (XConfigWrongTypeCastingException | XConfigKeyNotFoundException ignored) {
			return defaultValue;
		}
	}

	public Boolean getAsBoolean(String key, Boolean defaultValue) {
		try {
			return get(key).getAsBoolean();
		} catch (XConfigWrongTypeCastingException | XConfigKeyNotFoundException ignored) {
			return defaultValue;
		}
	}

	public XConfigList getAsList(String key, XConfigList defaultValue) {
		try {
			return get(key).getAsList();
		} catch (XConfigWrongTypeCastingException | XConfigKeyNotFoundException ignored) {
			return defaultValue;
		}
	}
	
	/**
	 * Merges the entries in otherMap into this map, prioritizing the entries in otherMap in case of conflict.
	 */
	public void overrideWith(XConfigMap otherMap) {
		Set<Entry<String, XConfigValue>> entrySet = otherMap.entrySet();
		for (Entry<String, XConfigValue> entry : entrySet) {
			XConfigValue value = entry.getValue();

			String key = entry.getKey();
			XConfigMap thisValue = null;
			XConfigMap otherValueMap = null;
			try {
				thisValue = this.get(key).getAsMap();
				otherValueMap = value.getAsMap();
			} catch (XConfigWrongTypeCastingException | XConfigKeyNotFoundException e) {
			}
			if (thisValue != null && otherValueMap != null) {
				thisValue.overrideWith(otherValueMap);
			} else {
				this.add(key, value);
			}
		}
	}

	/**
	 * Combines this map and {@code otherMap} into a new map.
	 * In case of conflict prioritizes the entries in {@code otherMap}.
	 */
	public XConfigMap mergeWith(XConfigMap otherMap) {
		Map<String, XConfigValue> resultMap = new HashMap<>(values);
		for (Entry<String, XConfigValue> entry : otherMap.values.entrySet()) {
			String key = entry.getKey();
			XConfigValue value = entry.getValue();
			if (value instanceof XConfigMap) {
				XConfigValue ownValue = values.get(key);
				if (ownValue instanceof XConfigMap) {
					value = ((XConfigMap) ownValue).mergeWith(((XConfigMap) value));
				}
			}
			resultMap.put(key, value);
		}

		// Avoids another copy of the map.
		XConfigMap xConfigMap = new XConfigMap();
		xConfigMap.values = resultMap;
		return xConfigMap;
	}
}
