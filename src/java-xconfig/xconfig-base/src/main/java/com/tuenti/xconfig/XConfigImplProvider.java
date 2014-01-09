package com.tuenti.xconfig;

public interface XConfigImplProvider {
	public XConfig create(final String path, final String socket, final boolean autoReload);

	/**
	 * Overloaded creation method with default autoReload parameter to false.
	 * 
	 * @param path
	 *            Config entry path
	 * @return XConfig instance
	 */
	public XConfig create(final String path);
}
