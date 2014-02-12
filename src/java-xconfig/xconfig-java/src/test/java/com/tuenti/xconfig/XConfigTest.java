/**
 * XConfigTest.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigValue;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * XConfigTest class
 */
public class XConfigTest {

	private XConfig xconfig = null;

	@Before
	public void setUp() throws Exception {
		String path = getClass().getResource("/yaml-test-files/").getPath();
		xconfig = new XConfigJavaProvider().create(path);
	}

	@After
	public void tearDown() {
		xconfig.close();
	}

	@Test(expected = XConfigKeyNotFoundException.class)
	public void testNonexistentKeyThrowsException() throws XConfigKeyNotFoundException {
		xconfig.getValue("nonexistentkey");
	}

	@Test(expected = XConfigKeyNotFoundException.class)
	public void testEndingSlashThrowsException() throws XConfigKeyNotFoundException {
		xconfig.getValue("basic/");
	}

	@Test(expected = XConfigKeyNotFoundException.class)
	public void testLastModificationTimeEndingSlashThrowsException()
			throws XConfigKeyNotFoundException {
		xconfig.getLastModificationTime("basic/");
	}

	@Test
	public void testRetrieveIntegerValueNode()
			throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		XConfigValue value = xconfig.getValue("basic/integerValue");
		Assert.assertEquals(123, (long) value.getAsInteger());
	}

	@Test
	public void testRetrieveStringValueNode()
			throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		XConfigValue value = xconfig.getValue("basic/stringValue");
		Assert.assertEquals("test", value.getAsString());
	}

	@Test
	public void testRetrieveFloatValueNode()
			throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		XConfigValue value = xconfig.getValue("basic/floatValue");
		Assert.assertEquals(3.14f, value.getAsFloat(), 0.0);
	}

	@Test
	public void testRetrieveBooleanValueNode()
			throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		XConfigValue value = xconfig.getValue("basic/booleanValue");
		Assert.assertFalse(value.getAsBoolean());
	}

	@Test
	public void testRetrieveIntegerValueNodeDirectlyFromXConfig()
			throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		String key = "basic/integerValue";
		XConfigValue value = xconfig.getValue(key);
		Assert.assertEquals(xconfig.getAsInteger(key), value.getAsInteger());
	}

	@Test
	public void testRetrieveStringValueNodeDirectlyFromXConfig()
			throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		String key = "basic/stringValue";
		XConfigValue value = xconfig.getValue(key);
		Assert.assertEquals(xconfig.getAsString(key), value.getAsString());
	}

	@Test
	public void testRetrieveFloatValueNodeDirectlyFromXConfig()
			throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		String key = "basic/floatValue";
		XConfigValue value = xconfig.getValue(key);
		Assert.assertEquals(xconfig.getAsFloat(key), value.getAsFloat(), 0.0);
	}

	@Test
	public void testRetrieveBooleanValueNodeDirectlyFromXConfig()
			throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		String key = "basic/booleanValue";
		XConfigValue value = xconfig.getValue(key);
		Assert.assertEquals(xconfig.getAsBoolean(key), value.getAsBoolean());
	}

	@Test
	public void testRetrieveNullValueNode()
			throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		XConfigValue value = xconfig.getValue("basic/nullValue");
		Assert.assertEquals(null, value.getAsInteger());
		Assert.assertEquals(null, value.getAsString());
		Assert.assertEquals(null, value.getAsFloat());
		Assert.assertEquals(null, value.getAsBoolean());
		Assert.assertEquals(null, value.getAsList());
		Assert.assertEquals(null, value.getAsMap());
	}

	@Test
	public void testReloadReturnsFalse()
			throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		Assert.assertFalse(xconfig.reload());
	}

	@Test
	public void testWalkThroughIntegersList()
			throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		XConfigValue value = xconfig.getValue("basic/integersList");
		List<XConfigValue> list = value.getAsList();
		Assert.assertEquals(5, list.size());
		for (int i = 0; i < 5; i++) {
			Integer intValue = list.get(i).getAsInteger();
			Assert.assertEquals(i, intValue.intValue());
		}
	}

	@Test
	public void testPureMapBasicBehaviour()
			throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		XConfigValue value = xconfig.getValue("basic/pureMap");
		Map<String, XConfigValue> map = value.getAsMap();
		Assert.assertEquals(3, map.keySet().size());
		for (int i = 1; i <= 3; i++) {
			String key = String.format("key%d", i);
			Assert.assertTrue(map.containsKey(key));
			Assert.assertEquals(String.format("test%d", i), map.get(key).getAsString());
		}
	}

	@Test
	public void testMixedMapBasicBehaviour()
			throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		XConfigValue value = xconfig.getValue("basic/mixedMap");
		Map<String, XConfigValue> map = value.getAsMap();
		Assert.assertEquals(4, map.keySet().size());
		value = map.get("key1");
		Assert.assertEquals("test1", value.getAsString());
		value = map.get("key2");
		Assert.assertEquals(2, value.getAsInteger().intValue());
		value = map.get("key3");
		Assert.assertEquals(1.16f, value.getAsFloat(), 0);
		value = map.get("key4");
		Assert.assertEquals(true, value.getAsBoolean().booleanValue());
	}

	@Test
	public void testGetAsStringReturnsDefaultValueWhenTypeCastingMismatch() {
		Assert.assertEquals("test", xconfig.getAsString("basic/integerValue", "test"));
	}

	@Test
	public void testGetAsIntegerReturnsDefaultValueWhenTypeCastingMismatch() {
		Assert.assertEquals(1010, (int)xconfig.getAsInteger("basic/stringValue", 1010));
	}

	@Test
	public void testGetAsFloatReturnsDefaultValueWhenTypeCastingMismatch() {
		Assert.assertEquals(1.23f, xconfig.getAsFloat("basic/stringValue", 1.23f), 0);
	}

	@Test
	public void testGetAsBooleanReturnsDefaultValueWhenTypeCastingMismatch() {
		Assert.assertEquals(Boolean.TRUE, xconfig.getAsBoolean("basic/stringValue", Boolean.TRUE));
	}

	@Test
	public void testGetAsListReturnsDefaultValueWhenTypeCastingMismatch() {
		List<XConfigValue> testList = new ArrayList<XConfigValue>();
		Assert.assertEquals(testList, xconfig.getAsList("basic/stringValue", testList));
	}

	@Test
	public void testGetAsMapReturnsDefaultValueWhenTypeCastingMismatch() {
		Map<String, XConfigValue> testMap = new HashMap<String, XConfigValue>();
		Assert.assertEquals(testMap, xconfig.getAsMap("basic/stringValue", testMap));
	}
}
