/**
 * XConfigStringUnitTest.java
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

public class XConfigStringUnitTest {

	private static final double ZERO_DELTA = 0;

	private XConfigString object;
	private String value;

	@Before
	public void setUp() {
		this.value = "test_string";
		this.object = new XConfigString(this.value);
	}

	@Test
	public void testGetAsStringReturnsExpectedString() {
		assertEquals(value, object.getAsString());
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsIntegerThrowsWrongTypeCastingException() {
		object.getAsInteger();
	}

	@Test
	public void testGetAsIntegerReturnsIntegerIfCorrectlyParsed() {
		assertEquals(1234, new XConfigString("1234").getAsInteger().intValue());
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsFloatThrowsWrongTypeCastingException() {
		object.getAsFloat();
	}

	@Test
	public void testGetAsFloatReturnsFloatIfCorrectlyParsed() {
		assertEquals(12.1f, new XConfigString("12.1").getAsFloat(), ZERO_DELTA);
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsLongThrowsWrongTypeCastingException() {
		object.getAsLong();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testExceedMaxIntegerThrowsWrongTypeCastingException() {
		new XConfigString("100200300400500").getAsInteger();
	}

	@Test
	public void testGetAsLongReturnsLongIfCorrectlyParsed() {
		assertEquals(new Long(100200300400500L), new XConfigString("100200300400500").getAsLong());
	}

	@Test
	public void testTrueBoolean() {
		assertEquals(true, new XConfigString("true").getAsBoolean());
	}

	@Test
	public void testFalseBoolean() {
		assertEquals(false, new XConfigString("false").getAsBoolean());
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testUnrecognizedBooleanStringThrowsWrongTypeCastingException() {
		new XConfigString("True").getAsBoolean();
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
		assertEquals(this.object.getAsString(), this.object.getAsJavaObject());
	}
}
