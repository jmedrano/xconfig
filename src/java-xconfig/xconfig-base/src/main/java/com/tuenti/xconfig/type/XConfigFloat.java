/*
 * XConfigFloat.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig.type;

import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;

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
		return this.value.toString();
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
		if (value > Integer.MAX_VALUE || value < Integer.MIN_VALUE) {
			throw new XConfigWrongTypeCastingException();
		}
		return value.intValue();
	}

	@Override
	public Long getAsLong() throws XConfigWrongTypeCastingException {
		if (value > Long.MAX_VALUE || value < Long.MIN_VALUE) {
			throw new XConfigWrongTypeCastingException();
		}
		return value.longValue();
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
		return XConfigValueType.FLOAT;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;

		XConfigFloat that = (XConfigFloat) o;

		if (!value.equals(that.value)) return false;

		return true;
	}

	@Override
	public int hashCode() {
		return value.hashCode();
	}

	@Override
	public Float getAsJavaObject() {
		return value;
	}
}
