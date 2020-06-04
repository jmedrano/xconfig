package com.tuenti.xconfig.cache;

import com.tuenti.xconfig.XConfig;

public interface XConfigCacheProvider {
	ReloadResult reloadCache(XConfig config, XConfigCache currentCache);

	class ReloadResult {
		public final boolean reloaded;
		public final XConfigCache cache;

		public ReloadResult(boolean reloaded, XConfigCache cache) {
			this.reloaded = reloaded;
			this.cache = cache;
		}
	}
}
