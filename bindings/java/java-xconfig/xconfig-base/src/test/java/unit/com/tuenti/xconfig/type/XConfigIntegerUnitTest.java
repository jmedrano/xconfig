/**
 * XConfigIntegerUnitTest.java
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

public class XConfigIntegerUnitTest {

	private XConfigInteger object;
	private Integer value;

	@Before
	public void setUp() throws Exception {
		this.value = 12345678;
		this.object = new XConfigInteger(this.value);
	}

	@Test
	public void testGetAsIntegerReturnsExpectedInteger() {
		assertEquals(value, object.getAsInteger());
	}

	@Test
	public void testGetAsFloatReturnsExpectedFloat() {
		assertEquals(new Float(value), object.getAsFloat());
	}

	@Test
	public void testGetAsLongReturnsExpectedLong() {
		assertEquals(new Long(value), object.getAsLong());
	}

	@Test
	public void testGetAsStringReturnsExpectedString() {
		assertEquals("12345678", object.getAsString());
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsBooleanThrowsWrongTypeCastingException() {
		object.getAsBoolean();
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
		assertEquals(this.object.getAsInteger(), this.object.getAsJavaObject());
	}
}
