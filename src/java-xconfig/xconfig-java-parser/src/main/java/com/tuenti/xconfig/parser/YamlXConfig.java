/*
 * Copyright (C) 2015 Tuenti Technologies S.L.
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */

package com.tuenti.xconfig.parser;

import com.tuenti.xconfig.XConfig;
import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigList;
import com.tuenti.xconfig.type.XConfigMap;
import com.tuenti.xconfig.type.XConfigValue;

/**
 * XConfig class implementation from Yaml data. This class isn't intended to be registered as
 * a XConfig provider, but rather for testing purposes.
 */
public class YamlXConfig implements XConfig {

	private ConfigParser config;
	
	public static YamlXConfig configFromYaml(String yamlString) {
		return new YamlXConfig(yamlString);
	}
	
	public static YamlXConfig configFromJavaObject(Object yamlData) {
		YamlXConfig xconfig = new YamlXConfig();
		xconfig.config.addYamlFromJavaObject(yamlData);
		return xconfig;
	}
	
	public YamlXConfig() {
		config = new ConfigParser();
	}
	
	public YamlXConfig(String yamlString) {
		config = new ConfigParser();
		config.addYamlString(yamlString);
	}
	
	protected ConfigParser getConfig() {
		return config;
	}

	protected void setConfig(ConfigParser config) {
		this.config = config;
	}

	@Override
	public Boolean getAsBoolean(final String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException {
		return this.getValue(key).getAsBoolean();
	}

	@Override
	public Boolean getAsBoolean(final String key, final Boolean defaultValue) {
		try {
			return this.getValue(key).getAsBoolean();
		} catch (XConfigKeyNotFoundException ignored) {
		} catch (XConfigWrongTypeCastingException ignored) {
		}

		return defaultValue;
	}

	@Override
	public Float getAsFloat(final String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException {
		return this.getValue(key).getAsFloat();
	}

	@Override
	public Float getAsFloat(final String key, final Float defaultValue) {
		try {
			return this.getValue(key).getAsFloat();
		} catch (XConfigKeyNotFoundException ignored) {
		} catch (XConfigWrongTypeCastingException ignored) {
		}

		return defaultValue;
	}

	@Override
	public Integer getAsInteger(final String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException {
		return this.getValue(key).getAsInteger();
	}

	@Override
	public Integer getAsInteger(final String key, final Integer defaultValue) {
		try {
			return this.getValue(key).getAsInteger();
		} catch (XConfigKeyNotFoundException ignored) {
		} catch (XConfigWrongTypeCastingException ignored) {
		}

		return defaultValue;
	}

	@Override
	public XConfigList getAsList(final String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException {
		return this.getValue(key).getAsList();
	}

	@Override
	public XConfigList getAsList(final String key, final XConfigList defaultValue) {
		try {
			return this.getValue(key).getAsList();
		} catch (XConfigKeyNotFoundException ignored) {
		} catch (XConfigWrongTypeCastingException ignored) {
		}

		return defaultValue;
	}

	@Override
	public XConfigMap getAsMap(final String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException {
		return this.getValue(key).getAsMap();
	}

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

	@Override
	public String getAsString(final String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException {
		return this.getValue(key).getAsString();
	}

	@Override
	public String getAsString(final String key, final String defaultValue) {
		try {
			return this.getValue(key).getAsString();
		} catch (XConfigKeyNotFoundException ignored) {
		} catch (XConfigWrongTypeCastingException ignored) {
		}

		return defaultValue;
	}

	@Override
	public void close() {
	}

	@Override
	public boolean reload() {
		return false;
	}

	@Override
	public XConfigValue getValue(String key) throws XConfigKeyNotFoundException {
		return config.getElement(key);
	}

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
			getValue(key);
		} catch (XConfigKeyNotFoundException e) {
			return false;
		}
		return true;
	}
}

