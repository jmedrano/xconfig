package com.tuenti.xconfig.testutils;

import java.util.HashMap;
import java.util.Map;

import com.tuenti.xconfig.XConfigBase;
import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.type.XConfigString;
import com.tuenti.xconfig.type.XConfigValue;

public class XConfigBaseTestable extends XConfigBase {
	
	private final Map<String, XConfigValue> values = new HashMap<>();
	
	public XConfigBaseTestable(String key, XConfigValue value) {
		values.put(key, value);
	}

	public XConfigBaseTestable(String ... keyValues) {
		for (int i=0; i<keyValues.length-1; i+=2) {
			values.put(keyValues[i], new XConfigString(keyValues[i+1]));
		}
	}

	@Override
	public void close() {}

	@Override
	public boolean reload() {
		return false;
	}

	@Override
	public XConfigValue getValue(String key) throws XConfigKeyNotFoundException {
		if (values.containsKey(key)) {
			return values.get(key);
		} else {
			throw new XConfigKeyNotFoundException(key);
		}
	}

	@Override
	public boolean hasKey(String key) {
		return (values.containsKey(key));
	}

	@Override
	public long getLastModificationTime(String key) throws XConfigKeyNotFoundException {
		return 0;
	}
}
