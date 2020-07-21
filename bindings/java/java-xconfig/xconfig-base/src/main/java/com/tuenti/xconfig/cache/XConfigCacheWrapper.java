package com.tuenti.xconfig.cache;

import com.tuenti.xconfig.XConfig;
import com.tuenti.xconfig.XConfigBase;
import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.type.XConfigValue;

public class XConfigCacheWrapper extends XConfigBase {
	private final XConfig wrapped;
	private final XConfigCacheProvider cacheProvider;
	private XConfigCache cache;

	public XConfigCacheWrapper(XConfig wrapped, XConfigCacheProvider cacheProvider) {
		this.wrapped = wrapped;
		this.cacheProvider = cacheProvider;
		cache = cacheProvider.reloadCache(wrapped, null).cache;
	}

	public XConfig unwrap() {
		return wrapped;
	}

	@Override
	public void close() {
		wrapped.close();
	}

	@Override
	public boolean reload() {
		XConfigCacheProvider.ReloadResult reloadResult = cacheProvider.reloadCache(wrapped, cache);
		cache = reloadResult.cache;

		return reloadResult.reloaded;
	}

	@Override
	public XConfigValue getValue(String key) throws XConfigKeyNotFoundException {
		return cache.getValue(key, wrapped);
	}

	@Override
	public boolean hasKey(String key) {
		// This is not implemented using wrapped.hasKey() so this method can use the cache w/o duplicating it
		// or doing a double fetch on miss, if hasKey were to change and calling it becomes necessary
		// this should be revisited.
		try {
			// getValue throws if the key is missing
			getValue(key);
			return true;
		} catch (XConfigKeyNotFoundException notFoundException) {
			return false;
		}
	}

	@Override
	public long getLastModificationTime(String key) throws XConfigKeyNotFoundException {
		return wrapped.getLastModificationTime(key);
	}

	@Override
	public long getHash(String key) throws XConfigKeyNotFoundException {
		return wrapped.getHash(key);
	}

	@Override
	public String toString() {
		return "XConfigCacheWrapper{" +
				"wrapped=" + wrapped +
				", cacheProvider=" + cacheProvider +
				", cache=" + cache +
				'}';
	}
}
