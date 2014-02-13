/**
 * XConfigMapUnitTest.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig.type;

import static org.junit.Assert.assertNull;

import org.junit.Before;
import org.junit.Test;

/**
 * XConfigMapUnitTest test class
 */
public class XConfigNullUnitTest {

	private XConfigNull object;

	@Before
	public void setUp() throws Exception {
		this.object = new XConfigNull();
	}

	@Test
	public void testGetAsListReturnsExpectedList() throws Exception {
		assertNull(this.object.getAsList());
	}

	@Test
	public void testGetAsStringReturnsExpectedString() throws Exception {
		assertNull(this.object.getAsString());
	}

	@Test
	public void testGetAsFloatReturnsExpectedFloat() throws Exception {
		assertNull(this.object.getAsFloat());
	}

	@Test
	public void testGetAsBooleanReturnsExpectedBoolean() throws Exception {
		assertNull(this.object.getAsBoolean());
	}

	@Test
	public void testGetAsIntegerReturnsExpectedBoolean() throws Exception {
		assertNull(this.object.getAsInteger());
	}

	@Test
	public void testGetAsJavaObject() throws Exception {
		assertNull(null, this.object.getAsJavaObject());
	}
}
