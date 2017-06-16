/*
 * XConfigInteger.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig.type;

import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;

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
	public String getAsString() throws XConfigWrongTypeCastingException {
		return this.value.toString();
	}

	@Override
	public Float getAsFloat() {
		return value.floatValue();
	}

	@Override
	public Boolean getAsBoolean() throws XConfigWrongTypeCastingException {
		throw new XConfigWrongTypeCastingException();
	}

	@Override
	public XConfigMap getAsMap() throws XConfigWrongTypeCastingException {
		throw new XConfigWrongTypeCastingException();
	}

	@Override
	public XConfigList getAsList() throws XConfigWrongTypeCastingException {
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

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;

		XConfigInteger that = (XConfigInteger) o;

		if (!value.equals(that.value)) return false;

		return true;
	}

	@Override
	public int hashCode() {
		return value.hashCode();
	}

	@Override
	public Integer getAsJavaObject() {
		return value;
	}
}
