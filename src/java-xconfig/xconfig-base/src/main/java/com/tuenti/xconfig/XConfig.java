package com.tuenti.xconfig;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigList;
import com.tuenti.xconfig.type.XConfigMap;
import com.tuenti.xconfig.type.XConfigValue;

public interface XConfig {

	/**
	 * Tries to recover a given key as a boolean value.
	 * 
	 * @param key
	 *            Config key
	 * @return Value as a boolean
	 */
	Boolean getAsBoolean(String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException;

	/**
	 * Tries to recover a given key as a boolean value if key is not found
	 * default value will be returned.
	 * 
	 * @param key
	 *            Config key
	 * @param defaultValue
	 *            Default value
	 * @return Value as a boolean
	 */
	Boolean getAsBoolean(String key, Boolean defaultValue);

	/**
	 * Tries to recover a given key as a float value.
	 * 
	 * @param key
	 *            Config key
	 * @return Value as a float
	 */
	Float getAsFloat(String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException;

	/**
	 * Tries to recover a given key as a float value if key is not found default
	 * value will be returned.
	 * 
	 * @param key
	 *            Config key
	 * @param defaultValue
	 *            Default value
	 * @return Value as a float
	 */
	Float getAsFloat(String key, Float defaultValue);

	/**
	 * Tries to recover a given key as an integer value.
	 * 
	 * @param key
	 *            Config key
	 * @return Value as an integer
	 */
	Integer getAsInteger(String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException;

	/**
	 * Tries to recover a given key as an integer value if key is not found
	 * default value will be returned.
	 * 
	 * @param key
	 *            Config key
	 * @param defaultValue
	 *            Default value
	 * @return Value as a float
	 */
	Integer getAsInteger(String key, Integer defaultValue);

	/**
	 * Tries to recover a given key as a XConfigValue's list.
	 * 
	 * @param key
	 *            Config key
	 * @return Value as a list of XConfigValues
	 */
	XConfigList getAsList(String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException;

	/**
	 * Tries to recover a given key as a XConfigValue's list and returns default
	 * value if key could not be found.
	 * 
	 * @param key
	 *            Config key
	 * @param defaultValue
	 *            Default value
	 * @return Value as a float
	 */
	XConfigList getAsList(String key, XConfigList defaultValue);

	/**
	 * Tries to recover a given key as a XConfigValue's map indexed by keys.
	 * 
	 * @param key
	 *            Config key
	 * @return Value as a map of XConfigValues with its keys
	 */
	XConfigMap getAsMap(String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException;

	/**
	 * Tries to recover a given key as a XConfigValue's map indexed by keys, if
	 * key is not found defaultValue will be returned.
	 * 
	 * @param key
	 *            Config key
	 * @param defaultValue
	 *            Default value
	 * @return Value as a float
	 */
	XConfigMap getAsMap(String key, XConfigMap defaultValue);

	/**
	 * Tries to recover a given key as a string value.
	 * 
	 * @param key
	 *            Config key
	 * @return Value as a string
	 */
	String getAsString(String key) throws XConfigKeyNotFoundException,
			XConfigWrongTypeCastingException;

	/**
	 * Tries to recover a given key as a string value. Will return default value
	 * if key couldn't be found.
	 * 
	 * @param key
	 *            Config key
	 * @param defaultValue
	 *            Default value
	 * @return Value as a string
	 */
	String getAsString(String key, String defaultValue);

	// Native methods from here

	/**
	 * Close connection and free all native instances created.
	 */
	void close();

	/**
	 * This will call the native reload method.
	 * 
	 * @return Whether reload was necessary or not.
	 */
	boolean reload();

	/**
	 * This will return one of the XConfigValue interface implementations
	 * depending on the node's type.
	 * 
	 * @param key
	 *            Config key
	 * @return XConfigValue object
	 */
	XConfigValue getValue(String key) throws XConfigKeyNotFoundException;

	/**
	 * Checks if the given key exists
	 * 
	 * @param key
	 * @return
	 */
	boolean hasKey(String key);

	/**
	 * Get last modification time for a given key in milliseconds.
	 * 
	 * @param key
	 *            Config key
	 * @return Timestamp in milliseconds
	 */
	long getLastModificationTime(String key) throws XConfigKeyNotFoundException;

}