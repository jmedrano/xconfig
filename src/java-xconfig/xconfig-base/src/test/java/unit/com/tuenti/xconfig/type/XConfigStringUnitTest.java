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

/**
 * XConfigStringUnitTest test class
 */
public class XConfigStringUnitTest {

	private static final double ZERO_DELTA = 0;

	private XConfigString object;
	private String value;

	@Before
	public void setUp() throws Exception {
		this.value = "test_string";
		this.object = new XConfigString(this.value);
	}

	@Test
	public void testGetAsStringReturnsExpectedString() throws Exception {
		assertEquals(value, object.getAsString());
	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsIntegerThrowsWrongTypeCastingException() throws Exception {
		object.getAsInteger();
	}

	@Test
	public void testGetAsIntegerReturnsIntegerIfCorrectlyParsed() throws Exception {
		assertEquals(1234, new XConfigString("1234").getAsInteger().intValue());
	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsFloatThrowsWrongTypeCastingException() throws Exception {
		object.getAsFloat();
	}

	@Test
	public void testGetAsFloatReturnsFloatIfCorrectlyParsed() throws Exception {
		assertEquals(12.1f, new XConfigString("12.1").getAsFloat(), ZERO_DELTA);
	}

	@Test
	public void testTrueBoolean() throws Exception {
		assertEquals(true, new XConfigString("true").getAsBoolean());
	}

	@Test
	public void testFalseBoolean() throws Exception {
		assertEquals(false, new XConfigString("false").getAsBoolean());
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testUnrecognizedBooleanStringThrowsWrongTypeCastingException() throws Exception {
		new XConfigString("True").getAsBoolean();
	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsBooleanThrowsWrongTypeCastingException() throws Exception {
		object.getAsBoolean();
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
		assertEquals(this.object.getAsString(), this.object.getAsJavaObject());
	}
}
