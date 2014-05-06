package com.tuenti.xconfig.type;



/**
 * Created by aaron on 13/12/13.
 */
public class XConfigNull implements XConfigValue {

	public XConfigNull() {}

	@Override
	public Integer getAsInteger() {
		return null;
	}

	@Override
	public String getAsString() {
		return null;
	}

	@Override
	public Float getAsFloat() {
		return null;
	}

	@Override
	public Boolean getAsBoolean() {
		return null;
	}

	@Override
	public XConfigMap getAsMap() {
		return new XConfigMap();
	}

	@Override
	public XConfigList getAsList() {
		return new XConfigList();
	}

	@Override
	public XConfigValueType getType() {
		return XConfigValueType.NULL;
	}

	@Override
	public String toString() {
		return "XConfigNull []";
	}

	@Override
	public Object getAsJavaObject() {
		return null;
	}
}
