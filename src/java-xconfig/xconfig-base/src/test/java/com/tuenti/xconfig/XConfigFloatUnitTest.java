/**
 * XConfigFloatUnitTest.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig;

import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigFloat;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * XConfigFloatUnitTest test class
 */
public class XConfigFloatUnitTest {

	private XConfigFloat object;
	private Float value;

	@Before
	public void setUp() throws Exception {
		this.value = 1.234f;
		this.object = new XConfigFloat(value);
	}

	@Test
	public void testGetAsFloatReturnsExpectedFloat() throws Exception {
		Assert.assertEquals(value, object.getAsFloat());
	}

	@Test
	public void testGetAsIntegerReturnsExpectedValue() throws Exception {
		Assert.assertEquals(value.intValue(), object.getAsInteger().intValue());
	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsStringThrowsWrongTypeCastingException() throws Exception {
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
