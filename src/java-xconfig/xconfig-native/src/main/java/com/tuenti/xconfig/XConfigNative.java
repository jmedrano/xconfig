/*
 * XConfig.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.type.XConfigValue;

/**
 * XConfig class implementation for java.
 */
public class XConfigNative extends XConfigBase {
	private long nativeHandle;
	private boolean connected = false;

	/**
	 * Private constructor.
	 * 
	 * @param path
	 *            Config entry path
	 * @param socket
	 *            XConfig's socket path (Default /srv/xconfig/server)
	 * @param autoReload
	 *            Whether automatic reload is enabled or not
	 */
	XConfigNative(final String path, final String socket, final boolean autoReload) {
		this.init(socket, path, autoReload);
		connected = true;
	}

	/**
	 * Initializes the XConfig connection given the socket as string and the
	 * path.
	 * 
	 * @param socket
	 *            XConfig's socket path (Default /srv/xconfig/server)
	 * @param path
	 *            Config entry path
	 * @param autoReload
	 *            Whether automatic reload is enabled or not
	 */
	private native void init(String socket, String path, boolean autoReload);

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.tuenti.xconfig.XConfig#close()
	 */
	@Override
	public synchronized void close() {
		if (connected) {
			closeConnection();
			connected = false;
		}
	}

	public native void closeConnection();

	public native void free();
	
	@Override
	protected void finalize() throws Throwable {
		close();
		free();
		super.finalize();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.tuenti.xconfig.XConfig#reload()
	 */
	@Override
	public native boolean reload();

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.tuenti.xconfig.XConfig#getValue(java.lang.String)
	 */
	@Override
	public native XConfigValue getValue(String key) throws XConfigKeyNotFoundException;

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.tuenti.xconfig.XConfig#getLastModificationTime(java.lang.String)
	 */
	@Override
	public native long getLastModificationTime(String key) throws XConfigKeyNotFoundException;

	@Override
	public boolean hasKey(String key) {
		try {
			getValue(key);
		} catch (XConfigKeyNotFoundException e) {
			return false;
		}
		return true;
	}
}
