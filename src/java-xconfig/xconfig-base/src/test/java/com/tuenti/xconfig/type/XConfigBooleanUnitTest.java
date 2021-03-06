/**
 * XConfigBooleanUnitTest.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig.type;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;

/**
 * XConfigBooleanUnitTest test class
 */
public class XConfigBooleanUnitTest {

	private XConfigBoolean object;
	private Boolean value;

	@Before
	public void setUp() throws Exception {
		this.value = true;
		this.object = new XConfigBoolean(value);
	}

	@Test
	public void testGetAsBooleanReturnsExpectedBoolean() throws Exception {
		assertEquals(value, object.getAsBoolean());
	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsStringThrowsWrongTypeCastingException() throws Exception {
		object.getAsString();
	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsFloatThrowsWrongTypeCastingException() throws Exception {
		object.getAsFloat();
	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsIntegerThrowsWrongTypeCastingException() throws Exception {
		object.getAsInteger();
	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsMapThrowsWrongTypeCastingException() throws Exception {
		object.getAsMap();
	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsListThrowsWrongTypeCastingException() throws Exception {
		object.getAsList();
	}

	@Test
	public void testGetAsJavaObject() throws Exception {
		assertEquals(this.object.getAsBoolean(), this.object.getAsJavaObject());
	}
}
