/**
 * XConfigIntegerUnitTest.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig;

import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigInteger;
import org.junit.*;

/**
 * XConfigIntegerUnitTest test class
 */
public class XConfigIntegerUnitTest {

	private XConfigInteger object;
	private Integer value;

	@Before
	public void setUp() throws Exception {
		this.value = 12345678;
		this.object = new XConfigInteger(this.value);
	}

	@Test
	public void testGetAsIntegerReturnsExpectedInteger() throws Exception {
		Assert.assertEquals(value, object.getAsInteger());
	}

	@Test
	public void testGetAsFloatReturnsExpectedFloat() throws Exception {
		Assert.assertEquals(new Float(value), object.getAsFloat());
	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsStringReturnsExpectedString() throws Exception {
		object.getAsString();
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
}
