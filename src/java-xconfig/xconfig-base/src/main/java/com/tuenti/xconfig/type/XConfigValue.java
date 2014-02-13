/*
 * XConfigValue.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig.type;

import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;

/**
 * XConfigValue interface.
 */
public interface XConfigValue {
	public Integer getAsInteger() throws XConfigWrongTypeCastingException;
	public String getAsString() throws XConfigWrongTypeCastingException;
	public Float getAsFloat() throws XConfigWrongTypeCastingException;
	public Boolean getAsBoolean() throws XConfigWrongTypeCastingException;
	public XConfigMap getAsMap() throws XConfigWrongTypeCastingException;
	public XConfigList getAsList() throws XConfigWrongTypeCastingException;
	public XConfigValueType getType();
	public Object getAsJavaObject();
}
