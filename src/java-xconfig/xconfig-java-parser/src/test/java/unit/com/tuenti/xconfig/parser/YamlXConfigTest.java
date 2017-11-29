/**
 * YamlXConfigTest.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.PrintWriter;
import java.io.StringWriter;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.tuenti.xconfig.XConfig;
import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.type.XConfigList;
import com.tuenti.xconfig.type.XConfigMap;
import com.tuenti.xconfig.type.XConfigValue;

public class YamlXConfigTest {

	private XConfig xconfig = null;

	@Before
	public void setUp() throws Exception {
		StringWriter outString = new StringWriter();
		PrintWriter out = new PrintWriter(outString, true);
		out.println("basic:");
		out.println("    'integerValue': 123");
		out.println("    'stringValue': 'test'");
		out.println("    'floatValue': 3.14");
		out.println("    'longValue': 100200300400500");
		out.println("    'booleanValue': false");
		out.println("    'nullValue': null");
		out.println("    'integersList':");
		out.println("      - 0");
		out.println("      - 1");
		out.println("      - 2");
		out.println("      - 3");
		out.println("      - 4");
		out.println("    'pureMap':");
		out.println("      'key1': 'test1'");
		out.println("      'key2': 'test2'");
		out.println("      'key3': 'test3'");
		out.println("    'mixedMap':");
		out.println("      'key1': 'test1'");
		out.println("      'key2': 2");
		out.println("      'key3': 1.16");
		out.println("      'key4': true");
		out.println("      'key5': 900800700600500");

		xconfig = new YamlXConfig(outString.toString());
	}

	@After
	public void tearDown() {
		xconfig.close();
	}

	@Test(expected = XConfigKeyNotFoundException.class)
	public void testNonexistentKeyThrowsException() {
		xconfig.getValue("nonexistentkey");
	}

	@Test(expected = XConfigKeyNotFoundException.class)
	public void testEndingSlashThrowsException() {
		xconfig.getValue("basic/");
	}

	@Test(expected = XConfigKeyNotFoundException.class)
	public void testLastModificationTimeEndingSlashThrowsException() {
		xconfig.getLastModificationTime("basic/");
	}

	@Test
	public void testRetrieveIntegerValueNode() {
		XConfigValue value = xconfig.getValue("basic/integerValue");
		assertEquals(123, (long) value.getAsInteger());
	}

	@Test
	public void testRetrieveStringValueNode() {
		XConfigValue value = xconfig.getValue("basic/stringValue");
		assertEquals("test", value.getAsString());
	}

	@Test
	public void testRetrieveFloatValueNode() {
		XConfigValue value = xconfig.getValue("basic/floatValue");
		assertEquals(3.14f, value.getAsFloat(), 0.0);
	}

	@Test
	public void testRetrieveLongValueNode() {
		XConfigValue value = xconfig.getValue("basic/longValue");
		assertEquals(new Long(100200300400500L), value.getAsLong());
	}

	@Test
	public void testRetrieveBooleanValueNode() {
		XConfigValue value = xconfig.getValue("basic/booleanValue");
		assertFalse(value.getAsBoolean());
	}

	@Test
	public void testRetrieveIntegerValueNodeDirectlyFromXConfig() {
		String key = "basic/integerValue";
		XConfigValue value = xconfig.getValue(key);
		assertEquals(xconfig.getAsInteger(key), value.getAsInteger());
	}

	@Test
	public void testRetrieveStringValueNodeDirectlyFromXConfig() {
		String key = "basic/stringValue";
		XConfigValue value = xconfig.getValue(key);
		assertEquals(xconfig.getAsString(key), value.getAsString());
	}

	@Test
	public void testRetrieveFloatValueNodeDirectlyFromXConfig() {
		String key = "basic/floatValue";
		XConfigValue value = xconfig.getValue(key);
		assertEquals(xconfig.getAsFloat(key), value.getAsFloat(), 0.0);
	}

	@Test
	public void testRetrieveBooleanValueNodeDirectlyFromXConfig() {
		String key = "basic/booleanValue";
		XConfigValue value = xconfig.getValue(key);
		assertEquals(xconfig.getAsBoolean(key), value.getAsBoolean());
	}

	@Test
	public void testRetrieveNullValueNode() {
		XConfigValue value = xconfig.getValue("basic/nullValue");
		assertNull(value.getAsInteger());
		assertNull(value.getAsString());
		assertNull(value.getAsFloat());
		assertNull(value.getAsBoolean());
		assertEquals(0, value.getAsList().size());
		assertEquals(0, value.getAsMap().size());
	}

	@Test
	public void testReloadReturnsFalse() {
		assertFalse(xconfig.reload());
	}

	@Test
	public void testWalkThroughIntegersList() {
		XConfigValue value = xconfig.getValue("basic/integersList");
		XConfigList list = value.getAsList();
		assertEquals(5, list.size());
		assertEquals(0, list.get(0).getAsInteger().intValue());
		assertEquals(1, list.get(1).getAsInteger().intValue());
		assertEquals(2, list.get(2).getAsInteger().intValue());
		assertEquals(3, list.get(3).getAsInteger().intValue());
		assertEquals(4, list.get(4).getAsInteger().intValue());
	}

	@Test
	public void testPureMapBasicBehaviour() {
		XConfigValue value = xconfig.getValue("basic/pureMap");
		XConfigMap map = value.getAsMap();
		assertEquals(3, map.keySet().size());
		assertTrue(map.containsKey("key1"));
		assertEquals("test1", map.get("key1").getAsString());
		assertTrue(map.containsKey("key2"));
		assertEquals("test2", map.get("key2").getAsString());
		assertTrue(map.containsKey("key3"));
		assertEquals("test3", map.get("key3").getAsString());
	}

	@Test
	public void testMixedMapBasicBehaviour() {
		XConfigValue value = xconfig.getValue("basic/mixedMap");
		XConfigMap map = value.getAsMap();
		assertEquals(5, map.keySet().size());
		assertEquals("test1", map.get("key1").getAsString());
		assertEquals(2, map.get("key2").getAsInteger().intValue());
		assertEquals(1.16f, map.get("key3").getAsFloat(), 0);
		assertEquals(true, map.get("key4").getAsBoolean().booleanValue());
		assertEquals(new Long(900800700600500L), map.get("key5").getAsLong());
	}

	@Test
	public void testGetAsStringReturnsDefaultValueWhenTypeCastingMismatch() {
		assertEquals("test", xconfig.getAsString("basic/integersList", "test"));
	}

	@Test
	public void testGetAsIntegerReturnsDefaultValueWhenTypeCastingMismatch() {
		assertEquals(1010, (int)xconfig.getAsInteger("basic/stringValue", 1010));
	}

	@Test
	public void testGetAsFloatReturnsDefaultValueWhenTypeCastingMismatch() {
		assertEquals(1.23f, xconfig.getAsFloat("basic/stringValue", 1.23f), 0);
	}

	@Test
	public void testGetAsLongReturnsDefaultValueWhenTypeCastingMismatch() {
		assertEquals(new Long(123L), xconfig.getAsLong("basic/stringValue", 123L));
	}

	@Test
	public void testGetAsBooleanReturnsDefaultValueWhenTypeCastingMismatch() {
		assertEquals(Boolean.TRUE, xconfig.getAsBoolean("basic/stringValue", Boolean.TRUE));
	}

	@Test
	public void testGetAsListReturnsDefaultValueWhenTypeCastingMismatch() {
		XConfigList testList = new XConfigList();
		assertEquals(testList, xconfig.getAsList("basic/stringValue", testList));
	}

	@Test
	public void testGetAsMapReturnsDefaultValueWhenTypeCastingMismatch() {
		XConfigMap testMap = new XConfigMap();
		assertEquals(testMap, xconfig.getAsMap("basic/stringValue", testMap));
	}
}
