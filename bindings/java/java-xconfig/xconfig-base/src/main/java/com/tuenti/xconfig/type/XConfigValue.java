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
	Integer getAsInteger() throws XConfigWrongTypeCastingException;
	String getAsString() throws XConfigWrongTypeCastingException;
	Float getAsFloat() throws XConfigWrongTypeCastingException;
	Long getAsLong() throws XConfigWrongTypeCastingException;
	Boolean getAsBoolean() throws XConfigWrongTypeCastingException;
	XConfigMap getAsMap() throws XConfigWrongTypeCastingException;
	XConfigList getAsList() throws XConfigWrongTypeCastingException;
	XConfigValueType getType();
	Object getAsJavaObject();
}
