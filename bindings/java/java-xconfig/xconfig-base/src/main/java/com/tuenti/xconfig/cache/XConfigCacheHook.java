package com.tuenti.xconfig.cache;

public interface XConfigCacheHook {
	default void onSetValue(String key, Object value) {
		//NOOP
	}

	default void onSetValueNotFound(String key) {
		//NOOP
	};

	default void onGetValue(String key, Object value) {
		//NOOP
	}

	default void onGetValueNotFound(String key) {
		//NOOP
	}
}
