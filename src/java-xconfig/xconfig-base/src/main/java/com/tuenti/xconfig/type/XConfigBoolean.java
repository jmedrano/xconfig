/*
 * XConfigBoolean.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig.type;

import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;

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
	public XConfigMap getAsMap() throws XConfigWrongTypeCastingException {
		throw new XConfigWrongTypeCastingException();
	}

	@Override
	public XConfigList getAsList() throws XConfigWrongTypeCastingException {
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

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;

		XConfigBoolean that = (XConfigBoolean) o;

		if (!value.equals(that.value)) return false;

		return true;
	}

	@Override
	public int hashCode() {
		return value.hashCode();
	}

	@Override
	public Boolean getAsJavaObject() {
		return value;
	}
}
