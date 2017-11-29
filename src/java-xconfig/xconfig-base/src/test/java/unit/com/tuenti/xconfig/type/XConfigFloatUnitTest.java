/**
 * XConfigFloatUnitTest.java
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

public class XConfigFloatUnitTest {

	private XConfigFloat object;
	private Float value;

	@Before
	public void setUp() {
		this.value = 1.234f;
		this.object = new XConfigFloat(value);
	}

	@Test
	public void testGetAsFloatReturnsExpectedFloat() {
		assertEquals(value, object.getAsFloat());
	}

	@Test
	public void testGetAsIntegerReturnsExpectedValue() {
		assertEquals(value.intValue(), object.getAsInteger().intValue());
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsIntegerAboveLimitThrowsWrongTypeCastingException() {
		float floatAboveIntegerLimit = ((float) Integer.MAX_VALUE) * 2;
		new XConfigFloat(floatAboveIntegerLimit).getAsInteger();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsIntegerBelowLimitThrowsWrongTypeCastingException() {
		float floatBelowIntegerLimit = ((float) Integer.MAX_VALUE) * -2;
		new XConfigFloat(floatBelowIntegerLimit).getAsInteger();
	}

	@Test
	public void testGetAsLongReturnsExpectedValue() {
		assertEquals(value.longValue(), object.getAsLong().longValue());
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsLongAboveLimitThrowsWrongTypeCastingException() {
		float floatAboveLongLimit = ((float) Long.MAX_VALUE) * 2;
		new XConfigFloat(floatAboveLongLimit).getAsLong();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsLongBelowLimitThrowsWrongTypeCastingException() {
		float floatBelowLongLimit = ((float) Long.MAX_VALUE) * -2;
		new XConfigFloat(floatBelowLongLimit).getAsLong();
	}

	@Test
	public void testGetAsStringReturnsExpectedValue() {
		assertEquals("1.234", object.getAsString());
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
		assertEquals(this.object.getAsFloat(), this.object.getAsJavaObject());
	}
}
