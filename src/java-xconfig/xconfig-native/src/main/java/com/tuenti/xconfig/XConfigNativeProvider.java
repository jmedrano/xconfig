package com.tuenti.xconfig;

public class XConfigNativeProvider implements XConfigImplProvider {
	public static final String DEFAULT_SOCKET = "/var/xconfig/server";

	/**
	 * Creation method for XConfig.
	 * 
	 * @param path
	 *            Config entry path
	 * @param socket
	 *            XConfig's socket path (Default /srv/xconfig/server)
	 * @param autoReload
	 *            Whether automatic reload is enabled or not
	 * @return XConfig instance
	 */
	@Override
	public XConfig create(final String path, final String socket, final boolean autoReload) {
		return new XConfigNative(path, socket, autoReload);
	}

	/**
	 * Overloaded creation method with default autoReload parameter to false.
	 * 
	 * @param path
	 *            Config entry path
	 * @return XConfig instance
	 */
	@Override
	public XConfig create(String path) {
		return new XConfigNative(path, DEFAULT_SOCKET, false);
	}

	static {
		NarSystem.loadLibrary();
	}
}
