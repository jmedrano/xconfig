package com.tuenti.xconfig.type;

import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;

public class XConfigLong implements XConfigValue {

	private Long value = null;

	public XConfigLong(long value) {
		this.value = value;
	}

	@Override
	public Long getAsLong() {
		return this.value;
	}

	@Override
	public Integer getAsInteger() throws XConfigWrongTypeCastingException {
		if (value > Integer.MAX_VALUE || value < Integer.MIN_VALUE) {
			throw new XConfigWrongTypeCastingException();
		}
		return value.intValue();
	}

	@Override
	public String getAsString() {
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
		return XConfigValueType.LONG;
	}

	@Override
	public String toString() {
		return "XConfigLong [value=" + value + "]";
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;

		XConfigLong that = (XConfigLong) o;

		if (!value.equals(that.value)) return false;

		return true;
	}

	@Override
	public int hashCode() {
		return value.hashCode();
	}

	@Override
	public Long getAsJavaObject() {
		return value;
	}
}
