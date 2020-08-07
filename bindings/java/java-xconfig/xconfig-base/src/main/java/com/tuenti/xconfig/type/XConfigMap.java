/*
 * XConfigMap.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig.type;

import java.util.Collection;
import java.util.Collections;
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

	private final Map<String, XConfigValue> values;
	private Map<String, XConfigValue> unmodifiableView;
	private int cachedHash;

	/**
	 * Creates a empty XConfigMap
	 */
	public XConfigMap() {
		this.values = Collections.emptyMap();
	}

	/**
	 * Creates a XConfigMap backed by a copy of the provided {@code map}.
	 */
	public XConfigMap(Map<String, XConfigValue> map) {
		this.values = new HashMap<>(map);
	}

	/**
	 * Creates a XConfigMap using the same instance of the {@code backingMap} as the source of values.
	 * Use {@link #XConfigMap(Map)} if you want it to use a copy.
	 */
	public static XConfigMap wrapping(Map<String, XConfigValue> backingMap) {
		return new XConfigMap(backingMap, null);
	}

	private XConfigMap(Map<String, XConfigValue> backingMap, @SuppressWarnings("unused") PrivateMarker marker) {
		this.values = backingMap;
	}

	private static class PrivateMarker {}

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

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;

		XConfigMap that = (XConfigMap) o;

		return values.equals(that.values);
	}

	@Override
	public int hashCode() {
		int hash = cachedHash;
		if (hash == 0) {
			hash = values.hashCode();
			cachedHash = hash;
		}
		return hash;
	}

	@Override
	public Map<String, Object> getAsJavaObject() {
		Map<String, Object> javaValues = new HashMap<>();
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

		return wrapping(resultMap);
	}

	/*
	 * Following methods return a unmodifiable view
	 */

	public Set<String> keySet() {
		return unmodifiableView().keySet();
	}

	public Collection<XConfigValue> values() {
		return unmodifiableView().values();
	}

	public Set<Map.Entry<String,XConfigValue>> entrySet() {
		return unmodifiableView().entrySet();
	}

	private Map<String, XConfigValue> unmodifiableView() {
		Map<String, XConfigValue> view = unmodifiableView;
		if (view == null) {
			view = Collections.unmodifiableMap(values);
			unmodifiableView = view;
		}
		return view;
	}
}
