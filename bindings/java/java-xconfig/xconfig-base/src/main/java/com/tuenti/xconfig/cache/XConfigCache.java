package com.tuenti.xconfig.cache;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.RemovalCause;
import com.tuenti.xconfig.XConfig;
import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigList;
import com.tuenti.xconfig.type.XConfigMap;
import com.tuenti.xconfig.type.XConfigValue;
import com.tuenti.xconfig.type.XConfigValueType;

public class XConfigCache {
	private static final long MAX_CACHE_ENTRIES = 30_000;

	private final Cache<String, XConfigValue> cache;
	private final long configHash;
	private final XConfigCacheHook hook;

	public XConfigCache(long configHash, XConfigCacheHook hook) {
		this.configHash = configHash;
		this.hook = hook;
		this.cache = Caffeine.newBuilder()
				.maximumSize(MAX_CACHE_ENTRIES)
				.removalListener(this::removalListener)
				.build();
	}

	public XConfigValue getValue(String key, XConfig provider) {
		XConfigValue value = cache.get(key, s -> valueLoader(s, provider));
		if (value instanceof XConfigValueNotFoundMarker) {
			throw ((XConfigValueNotFoundMarker) value).exception;
		}
		return value;
	}

	private XConfigValue valueLoader(String key, XConfig provider) {
		XConfigValue value;
		try {
			value = provider.getValue(key);
			hook.onSetValue(key, value);
		} catch (XConfigKeyNotFoundException notFoundException) {
			value = new XConfigValueNotFoundMarker(notFoundException);
			hook.onSetValueNotFound(key);
		}
		return value;
	}

	public boolean isCacheValidForCurrentConfig(long currentConfigHash) {
		return currentConfigHash == configHash;
	}

	@Override
	public String toString() {
		return "XConfigCache{" +
				"identity=" + Integer.toHexString(System.identityHashCode(this)) +
				", configHash=" + Long.toHexString(configHash) +
				'}';
	}

	private void removalListener(String key, XConfigValue value, RemovalCause removalCause) {
		if (removalCause.wasEvicted()) {
			hook.onValueEvicted(key, value);
		}
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
