package com.tuenti.xconfig;

import java.util.List;
import java.util.Map;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigValue;

public interface XConfig {

	/**
	 * Tries to recover a given key as a boolean value.
	 * @param key Config key
	 * @return Value as a boolean
	 */
	public abstract Boolean getAsBoolean(String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException;

	/**
	 * Tries to recover a given key as a boolean value if key is not found default value will be returned.
	 * @param key Config key
	 * @param defaultValue Default value
	 * @return Value as a boolean
	 */
	public abstract Boolean getAsBoolean(String key, Boolean defaultValue);

	/**
	 * Tries to recover a given key as a float value.
	 * @param key Config key
	 * @return Value as a float
	 */
	public abstract Float getAsFloat(String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException;

	/**
	 * Tries to recover a given key as a float value if key is not found default value will be returned.
	 * @param key Config key
	 * @param defaultValue Default value
	 * @return Value as a float
	 */
	public abstract Float getAsFloat(String key, Float defaultValue);

	/**
	 * Tries to recover a given key as an integer value.
	 * @param key Config key
	 * @return Value as an integer
	 */
	public abstract Integer getAsInteger(String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException;

	/**
	 * Tries to recover a given key as an integer value if key is not found default value will be returned.
	 * @param key Config key
	 * @param defaultValue Default value
	 * @return Value as a float
	 */
	public abstract Integer getAsInteger(String key, Integer defaultValue);

	/**
	 * Tries to recover a given key as a XConfigValue's list.
	 * @param key Config key
	 * @return Value as a list of XConfigValues
	 */
	public abstract List<XConfigValue> getAsList(String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException;

	/**
	 * Tries to recover a given key as a XConfigValue's list and returns default value if key could not be found.
	 * @param key Config key
	 * @param defaultValue Default value
	 * @return Value as a float
	 */
	public abstract List<XConfigValue> getAsList(String key, List<XConfigValue> defaultValue);

	/**
	 * Tries to recover a given key as a XConfigValue's map indexed by keys.
	 * @param key Config key
	 * @return Value as a map of XConfigValues with its keys
	 */
	public abstract Map<String, XConfigValue> getAsMap(String key)
			throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException;

	/**
	 * Tries to recover a given key as a XConfigValue's map indexed by keys, if key is not found defaultValue will be returned.
	 * @param key Config key
	 * @param defaultValue Default value
	 * @return Value as a float
	 */
	public abstract Map<String, XConfigValue> getAsMap(String key, Map<String, XConfigValue> defaultValue);

	/**
	 * Tries to recover a given key as a string value.
	 * @param key Config key
	 * @return Value as a string
	 */
	public abstract String getAsString(String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException;

	/**
	 * Tries to recover a given key as a string value. Will return default value if key couldn't be found.
	 * @param key Config key
	 * @param defaultValue Default value
	 * @return Value as a string
	 */
	public abstract String getAsString(String key, String defaultValue);

	// Native methods from here

	/**
	 * Close connection and free all native instances created.
	 */
	public abstract void close();

	/**
	 * This will call the native reload method.
	 * @return Whether reload was necessary or not.
	 */
	public abstract boolean reload();

	/**
	 * This will return one of the XConfigValue interface implementations depending on the node's type.
	 * @param key Config key
	 * @return XConfigValue object
	 */
	public abstract XConfigValue getValue(String key) throws XConfigKeyNotFoundException;

	/**
	 * Get last modification time for a given key in milliseconds.
	 * @param key Config key
	 * @return Timestamp in milliseconds
	 */
	public abstract long getLastModificationTime(String key) throws XConfigKeyNotFoundException;

}