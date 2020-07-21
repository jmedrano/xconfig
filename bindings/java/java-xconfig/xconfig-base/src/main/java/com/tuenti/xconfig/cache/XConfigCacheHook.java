package com.tuenti.xconfig.cache;

import com.tuenti.xconfig.type.XConfigValue;

public interface XConfigCacheHook {
	default void onSetValue(String key, Object value) {
		//NOOP
	}

	default void onSetValueNotFound(String key) {
		//NOOP
	}

	default void onValueEvicted(String key, XConfigValue value) {
		//NOOP
	}
}
