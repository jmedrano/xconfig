package com.tuenti.xconfig.cache;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigList;
import com.tuenti.xconfig.type.XConfigMap;
import com.tuenti.xconfig.type.XConfigValue;
import com.tuenti.xconfig.type.XConfigValueType;

import java.util.concurrent.ConcurrentHashMap;

public class XConfigCache {
	private final ConcurrentHashMap<String, XConfigValue> valueMap = new ConcurrentHashMap<>();
	private final long configHash;
	private final XConfigCacheHook hook;

	public XConfigCache(long configHash, XConfigCacheHook hook) {
		this.configHash = configHash;
		this.hook = hook;
	}

	public XConfigValue getValue(String key) {
		XConfigValue value = valueMap.get(key);
		if (value instanceof XConfigValueNotFoundMarker) {
			hook.onGetValueNotFound(key);
			throw ((XConfigValueNotFoundMarker) value).exception;
		}

		hook.onGetValue(key, value);
		return value;
	}

	public void setValue(String key, XConfigValue value) {
		valueMap.put(key, value);
		hook.onSetValue(key, value);
	}

	public void setValue(String key, XConfigKeyNotFoundException notFoundException) {
		valueMap.put(key, new XConfigValueNotFoundMarker(notFoundException));
		hook.onSetValueNotFound(key);
	}

	public boolean isCacheValidForCurrentConfig(long currentConfigHash) {
		return currentConfigHash == configHash;
	}

	@Override
	public String toString() {
		return "XConfigCache{" +
				"identity=" + Integer.toHexString(System.identityHashCode(this)) +
				"configHash=" + Long.toHexString(configHash) +
				'}';
	}

	private static class XConfigValueNotFoundMarker implements XConfigValue {
		private final XConfigKeyNotFoundException exception;

		private XConfigValueNotFoundMarker(XConfigKeyNotFoundException exception) {
			this.exception = exception;
		}

		// Dummy impl
		@Override
		public Integer getAsInteger() throws XConfigWrongTypeCastingException {
			return null;
		}

		@Override
		public String getAsString() throws XConfigWrongTypeCastingException {
			return null;
		}

		@Override
		public Float getAsFloat() throws XConfigWrongTypeCastingException {
			return null;
		}

		@Override
		public Long getAsLong() throws XConfigWrongTypeCastingException {
			return null;
		}

		@Override
		public Boolean getAsBoolean() throws XConfigWrongTypeCastingException {
			return null;
		}

		@Override
		public XConfigMap getAsMap() throws XConfigWrongTypeCastingException {
			return null;
		}

		@Override
		public XConfigList getAsList() throws XConfigWrongTypeCastingException {
			return null;
		}

		@Override
		public XConfigValueType getType() {
			return null;
		}

		@Override
		public Object getAsJavaObject() {
			return null;
		}
	}
}
