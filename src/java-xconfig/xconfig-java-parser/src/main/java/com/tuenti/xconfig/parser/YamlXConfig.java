/*
 * Copyright (C) 2015 Tuenti Technologies S.L.
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */

package com.tuenti.xconfig.parser;

import com.tuenti.xconfig.XConfigBase;
import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.type.XConfigValue;

/**
 * XConfig class implementation from Yaml data. This class isn't intended to be registered as
 * a XConfig provider, but rather for testing purposes.
 */
public class YamlXConfig extends XConfigBase {

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

