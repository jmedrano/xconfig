/*
 * XConfig.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigList;
import com.tuenti.xconfig.type.XConfigMap;
import com.tuenti.xconfig.type.XConfigValue;

/**
 * XConfig class implementation for java.
 */
public final class XConfigNative implements XConfig {
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.tuenti.xconfig.XConfig#getAsBoolean(java.lang.String)
	 */
	@Override
	public Boolean getAsBoolean(final String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException {
		return this.getValue(key).getAsBoolean();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see com.tuenti.xconfig.XConfig#getAsBoolean(java.lang.String,
	 * java.lang.Boolean)
	 */
	@Override
	public Boolean getAsBoolean(final String key, final Boolean defaultValue) {
		try {
			return this.getValue(key).getAsBoolean();
		} catch (XConfigKeyNotFoundException ignored) {
		} catch (XConfigWrongTypeCastingException ignored) {
		}

		return defaultValue;
	}

    /*
	 * (non-Javadoc)
	 *
	 * @see com.tuenti.xconfig.XConfig#getAsFloat(java.lang.String)
	 */
	@Override
	public Float getAsFloat(final String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException {
		return this.getValue(key).getAsFloat();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.tuenti.xconfig.XConfig#getAsFloat(java.lang.String,
	 * java.lang.Float)
	 */
	@Override
	public Float getAsFloat(final String key, final Float defaultValue) {
		try {
			return this.getValue(key).getAsFloat();
		} catch (XConfigKeyNotFoundException ignored) {
		} catch (XConfigWrongTypeCastingException ignored) {
		}

		return defaultValue;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.tuenti.xconfig.XConfig#getAsInteger(java.lang.String)
	 */
	@Override
	public Integer getAsInteger(final String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException {
		return this.getValue(key).getAsInteger();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.tuenti.xconfig.XConfig#getAsInteger(java.lang.String,
	 * java.lang.Integer)
	 */
	@Override
	public Integer getAsInteger(final String key, final Integer defaultValue) {
		try {
			return this.getValue(key).getAsInteger();
		} catch (XConfigKeyNotFoundException ignored) {
		} catch (XConfigWrongTypeCastingException ignored) {
		}

		return defaultValue;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.tuenti.xconfig.XConfig#getAsList(java.lang.String)
	 */
	@Override
	public XConfigList getAsList(final String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException {
		return this.getValue(key).getAsList();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.tuenti.xconfig.XConfig#getAsList(java.lang.String,
	 * java.util.List)
	 */
	@Override
	public XConfigList getAsList(final String key, final XConfigList defaultValue) {
		try {
			return this.getValue(key).getAsList();
		} catch (XConfigKeyNotFoundException ignored) {
		} catch (XConfigWrongTypeCastingException ignored) {
		}

		return defaultValue;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.tuenti.xconfig.XConfig#getAsMap(java.lang.String)
	 */
	@Override
	public XConfigMap getAsMap(final String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException {
		return this.getValue(key).getAsMap();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.tuenti.xconfig.XConfig#getAsMap(java.lang.String, java.util.Map)
	 */
	@Override
	public XConfigMap getAsMap(final String key,
			final XConfigMap defaultValue) {
		try {
			return this.getValue(key).getAsMap();
		} catch (XConfigKeyNotFoundException ignored) {
		} catch (XConfigWrongTypeCastingException ignored) {
		}

		return defaultValue;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.tuenti.xconfig.XConfig#getAsString(java.lang.String)
	 */
	@Override
	public String getAsString(final String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException {
		return this.getValue(key).getAsString();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.tuenti.xconfig.XConfig#getAsString(java.lang.String,
	 * java.lang.String)
	 */
	@Override
	public String getAsString(final String key, final String defaultValue) {
		try {
			return this.getValue(key).getAsString();
		} catch (XConfigKeyNotFoundException ignored) {
		} catch (XConfigWrongTypeCastingException ignored) {
		}

		return defaultValue;
	}

	// Native methods from here

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
