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

import com.tuenti.xconfig.parser.ConfigParser;
import com.tuenti.xconfig.parser.YamlXConfig;

/**
 * XConfig class implementation for java.
 */
public class XConfigJava extends YamlXConfig {
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
		setConfig(loadConfig());
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

	@Override
	public void close() {
	}

	@Override
	public boolean reload() {
		long configTime = getConfigTime();
		if (configTime > getConfig().getLastModified()) {
			setConfig(loadConfig());
			return true;
		}
		return false;
	}
}
