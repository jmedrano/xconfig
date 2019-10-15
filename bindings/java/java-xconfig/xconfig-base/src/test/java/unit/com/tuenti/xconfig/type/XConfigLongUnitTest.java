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

public class XConfigLongUnitTest {

	private XConfigLong object;
	private Long value;

	@Before
	public void setUp() {
		this.value = 123L;
		this.object = new XConfigLong(this.value);
	}

	@Test
	public void testGetAsLongReturnsExpectedLong() {
		assertEquals(value, object.getAsLong());
	}

	@Test
	public void testGetAsIntegerReturnsExpectedInteger() {
		assertEquals(new Integer(123), object.getAsInteger());
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsIntegerAboveMaxThrowsWrongTypeCastingException() {
		long longAboveIntegerLimit = ((long) Integer.MAX_VALUE) + 1;
		new XConfigLong(longAboveIntegerLimit).getAsInteger();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsIntegerBelowMinThrowsWrongTypeCastingException() {
		long longBelowIntegerLimit = ((long) Integer.MIN_VALUE) - 1;
		new XConfigLong(longBelowIntegerLimit).getAsInteger();
	}

	@Test
	public void testGetAsFloatReturnsExpectedFloat() {
		assertEquals(new Float(value), object.getAsFloat());
	}

	@Test
	public void testGetAsStringReturnsExpectedString() {
		assertEquals("123", object.getAsString());
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
		assertEquals(this.object.getAsLong(), this.object.getAsJavaObject());
	}
}
