/**
 * XConfigMapUnitTest.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig.type;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Before;
import org.junit.Test;

public class XConfigNullUnitTest {

	private XConfigNull object;

	@Before
	public void setUp() {
		this.object = new XConfigNull();
	}

	@Test
	public void testGetAsListReturnsExpectedList() {
		assertEquals(0, this.object.getAsList().size());
	}

	@Test
	public void testGetAsMapReturnsExpectedMap() {
		assertEquals(0, this.object.getAsMap().size());
	}

	@Test
	public void testGetAsStringReturnsExpectedString() {
		assertNull(this.object.getAsString());
	}

	@Test
	public void testGetAsFloatReturnsExpectedFloat() {
		assertNull(this.object.getAsFloat());
	}

	@Test
	public void testGetAsBooleanReturnsExpectedBoolean() {
		assertNull(this.object.getAsBoolean());
	}

	@Test
	public void testGetAsIntegerReturnsExpectedInteger() {
		assertNull(this.object.getAsInteger());
	}

	@Test
	public void testGetAsLongReturnsExpectedLong() {
		assertNull(this.object.getAsLong());
	}

	@Test
	public void testGetAsJavaObject() {
		assertNull(null, this.object.getAsJavaObject());
	}
}
