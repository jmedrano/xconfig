package com.tuenti.xconfig;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigList;
import com.tuenti.xconfig.type.XConfigMap;
import com.tuenti.xconfig.type.XConfigValueType;

abstract public class XConfigBase implements XConfig {

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
		} catch (XConfigKeyNotFoundException | XConfigWrongTypeCastingException ignored) {
			return defaultValue;
		}
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
		} catch (XConfigKeyNotFoundException | XConfigWrongTypeCastingException ignored) {
			return defaultValue;
		}
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
		} catch (XConfigKeyNotFoundException | XConfigWrongTypeCastingException ignored) {
			return defaultValue;
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see com.tuenti.xconfig.XConfig#getAsLong(java.lang.String)
	 */
	@Override
	public Long getAsLong(final String key) throws XConfigKeyNotFoundException,
	XConfigWrongTypeCastingException {
		return this.getValue(key).getAsLong();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see com.tuenti.xconfig.XConfig#getAsLong(java.lang.String, java.lang.Long)
	 */
	@Override
	public Long getAsLong(final String key, final Long defaultValue) {
		try {
			return this.getValue(key).getAsLong();
		} catch (XConfigKeyNotFoundException | XConfigWrongTypeCastingException ignored) {
			return defaultValue;
		}
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
		} catch (XConfigKeyNotFoundException | XConfigWrongTypeCastingException ignored) {
			return defaultValue;
		}
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
		} catch (XConfigKeyNotFoundException | XConfigWrongTypeCastingException ignored) {
			return defaultValue;
		}
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
		} catch (XConfigKeyNotFoundException | XConfigWrongTypeCastingException ignored) {
			return defaultValue;
		}
	}

	@Override
	public boolean hasValidKey(String key, XConfigValueType type) {
		return hasKey(key) && getValue(key).getType() == type;
	}

	@SuppressWarnings("deprecation")
	@Override
	public long getHash(String key) throws XConfigKeyNotFoundException {
		return getLastModificationTime(key);
	}
}