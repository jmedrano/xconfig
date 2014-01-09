package com.tuenti.xconfig.type;

import java.util.List;
import java.util.Map;

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
    public Map<String, XConfigValue> getAsMap() {
        return null;
    }

    @Override
    public List<XConfigValue> getAsList() {
        return null;
    }

    @Override
    public XConfigValueType getType() {
        return XConfigValueType.NULL;
    }

	@Override
	public String toString() {
		return "XConfigNull []";
	}
}
