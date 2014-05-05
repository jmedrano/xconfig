/*
 * XConfig.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig;

import java.io.File;
import java.io.FileNotFoundException;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigList;
import com.tuenti.xconfig.type.XConfigMap;
import com.tuenti.xconfig.type.XConfigValue;

/**
 * XConfig class implementation for java.
 */
public class XConfigJava implements XConfig {
	private ConfigParser config = new ConfigParser();
	private String[] paths;

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
	XConfigJava(final String path, final String socket, final boolean autoReload) {
		this.paths = path.split(File.pathSeparator);
		config = loadConfig();
	}

	private long getConfigTime() {
		long lastModified = 0;
		for (String dir : paths) {
			File[] files = getFiles(dir);
			for (File file : files) {
				if (file.getName().endsWith(".yaml")) {
					lastModified = Math.max(lastModified, file.lastModified());
				}
			}
		}
		return lastModified;
	}

	private File[] getFiles(String dir) {
		File folder = new File(dir);
		if (!folder.exists()) {
			throw new RuntimeException("Directory " + folder + " doesn't exist");
		}
		File[] files = folder.listFiles();
		return files;
	}

	private ConfigParser loadConfig() {
		ConfigParser newConfig = new ConfigParser();
		for (String dir : paths) {
			File[] files = getFiles(dir);
			for (File file : files) {
				if (file.getName().endsWith(".yaml")) {
					try {
						newConfig.addFile(file);
					} catch (FileNotFoundException e) {
						e.printStackTrace();
					}
				}
			}
		}
		return newConfig;
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.tuenti.xconfig.XConfig#close()
	 */
	@Override
	public void close() {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.tuenti.xconfig.XConfig#reload()
	 */
	@Override
	public boolean reload() {
		long configTime = getConfigTime();
		if (configTime > config.getLastModified()) {
			config = loadConfig();
			return true;
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.tuenti.xconfig.XConfig#getValue(java.lang.String)
	 */
	@Override
	public XConfigValue getValue(String key) throws XConfigKeyNotFoundException {
		return config.getElement(key);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.tuenti.xconfig.XConfig#getLastModificationTime(java.lang.String)
	 */
	@Override
	public long getLastModificationTime(String key) throws XConfigKeyNotFoundException {
		// verify that the element exists
		config.getElement(key);
		//but simply return the config's last time
		return config.getLastModified();
	}

	@Override
	public boolean hasKey(String key) {
		try {
			config.getElement(key);
		} catch (XConfigKeyNotFoundException e) {
			return false;
		}
		return true;
	}
}
