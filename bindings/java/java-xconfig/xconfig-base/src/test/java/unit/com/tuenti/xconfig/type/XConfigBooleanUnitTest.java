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

public class XConfigBooleanUnitTest {

	private XConfigBoolean object;
	private Boolean value;

	@Before
	public void setUp() {
		this.value = true;
		this.object = new XConfigBoolean(value);
	}

	@Test
	public void testGetAsBooleanReturnsExpectedBoolean() {
		assertEquals(value, object.getAsBoolean());
	}

	@Test
	public void testGetAsStringReturnsExpectedString() {
		assertEquals("true", object.getAsString());
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsFloatThrowsWrongTypeCastingException() {
		object.getAsFloat();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsIntegerThrowsWrongTypeCastingException() {
		object.getAsInteger();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsLongThrowsWrongTypeCastingException() {
		object.getAsLong();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsMapThrowsWrongTypeCastingException() {
		object.getAsMap();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsListThrowsWrongTypeCastingException() {
		object.getAsList();
	}

	@Test
	public void testGetAsJavaObject() {
		assertEquals(this.object.getAsBoolean(), this.object.getAsJavaObject());
	}
}
