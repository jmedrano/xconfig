/**
 * XConfigMapUnitTest.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig.type;

import static org.junit.Assert.*;

import java.util.HashMap;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;

/**
 * XConfigMapUnitTest test class
 */
public class XConfigMapUnitTest {

	private XConfigMap object;
	private Map<String, XConfigValue> internalMap;
	private XConfigValue integerValue;

	@Before
	public void setUp() throws Exception {
		integerValue = new XConfigInteger(1);
		internalMap = new HashMap<String, XConfigValue>();
		internalMap.put("key1", integerValue);
		object = new XConfigMap(internalMap);
	}

	@Test
	public void testGetAsMapReturnsExpectedObject() throws Exception {
		XConfigMap map = object.getAsMap();
		assertEquals(object, map);
	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsStringThrowsWrongTypeCastingException() throws Exception {
		object.getAsString();
	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsFloatThrowsWrongTypeCastingException() throws Exception {
		object.getAsFloat();
	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsBooleanThrowsWrongTypeCastingException() throws Exception {
		object.getAsBoolean();
	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsIntegerThrowsWrongTypeCastingException() throws Exception {
		object.getAsInteger();
	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsListThrowsWrongTypeCastingException() throws Exception {
		object.getAsList();
	}

	@Test
	public void testSizeReturnsExpectedSize() {
		assertEquals(1, object.size());
	}

	@Test
	public void testIsEmptyReturnsExpectedResult() {
		assertFalse(object.isEmpty());
	}

	@Test
	public void testContainsKeyReturnsTrueForExistingKey() {
		assertTrue(object.containsKey("key1"));
	}

	@Test
	public void testContainsKeyReturnsFalseForNonExistingKey() {
		assertFalse(object.containsKey("non-existent"));
	}

	@Test
	public void testContainsValueReturnsTrueForExistingKey() {
		assertTrue(object.containsValue(integerValue));
	}

	@Test
	public void testContainsValueReturnsFalseForNonExistingKey() {
		assertFalse(object.containsValue(new XConfigInteger(2)));
	}

	@Test (expected = XConfigKeyNotFoundException.class)
	public void testGetThrowsExceptionWhenNotFound() throws Exception {
		object.get("non-existent");
	}

	@Test
	public void testKeySetReturnsAllKeys() throws Exception {
		assertEquals(internalMap.keySet(), object.keySet());
	}

	@Test
	public void testValuesReturnsAllValues() throws Exception {
		assertTrue(object.values().containsAll(internalMap.values()));
		assertTrue(internalMap.values().containsAll(object.values()));
	}

	@Test
	public void testEntrySetReturnsAllEntriesSet() throws Exception {
		assertEquals(internalMap.entrySet(), object.entrySet());
	}

	@Test
	public void testGetAsJavaObject() throws Exception {
		this.object.add("key2", new XConfigString("asd"));
		Map<String, Object> expectedMap = new HashMap<String, Object>();
		expectedMap.put("key1", new Integer(1));
		expectedMap.put("key2", "asd");
		assertEquals(expectedMap, this.object.getAsJavaObject());
	}

    @Test
    public void testGetAsStringWithKeyAndDefaultValueReturnsInternalValue() throws Exception {
        object.add("key2", new XConfigString("string"));
        assertEquals("string", object.getAsString("key2", null));
    }
    @Test
    public void testGetAsIntegerWithKeyAndDefaultValueReturnsInternalValue() throws Exception {
        assertEquals((Integer) 1, object.getAsInteger("key1", null));
    }
    @Test
    public void testGetAsFloatWithKeyAndDefaultValueReturnsInternalValue() throws Exception {
        object.add("key2", new XConfigFloat(1));
        assertEquals((Float) 1f, object.getAsFloat("key2", null));
    }
    @Test
    public void testGetAsBooleanWithKeyAndDefaultValueReturnsInternalValue() throws Exception {
        object.add("key2", new XConfigBoolean(true));
        assertEquals((Boolean) true, object.getAsBoolean("key2", null));
    }
    @Test
    public void testGetAsMApWithKeyAndDefaultValueReturnsInternalValue() throws Exception {
        XConfigMap map = new XConfigMap();
        object.add("key2", map);
        assertEquals(map, object.getAsMap("key2", null));
    }
    @Test
    public void testGetAsListWithKeyAndDefaultValueReturnsInternalValue() throws Exception {
        XConfigList list = new XConfigList();
        object.add("key2", list);
        assertEquals(list, object.getAsList("key2", null));
    }

    @Test
    public void testGetAsStringWithMissingKeyAndDefaultValueReturnsDefaultValue() throws Exception {
        assertEquals("default", object.getAsString("key2", "default"));
    }
    @Test
    public void testGetAsIntegerWithMissingKeyAndDefaultValueReturnsDefaultValue() throws Exception {
        assertEquals((Integer) 2, object.getAsInteger("key2", (Integer)2));
    }
    @Test
    public void testGetAsFloatWithMissingKeyAndDefaultValueReturnsDefaultValue() throws Exception {
        assertEquals((Float) 2f, object.getAsFloat("key2", (Float)2f));
    }
    @Test
    public void testGetAsBooleanWithMissingKeyAndDefaultValueReturnsDefaultValue() throws Exception {
        assertEquals((Boolean) false, object.getAsBoolean("key2", false));
    }
    @Test
    public void testGetAsMapWithMissingKeyAndDefaultValueReturnsDefaultValue() throws Exception {
        XConfigMap map = new XConfigMap();
        assertEquals(map, object.getAsMap("key2", map));
    }
    @Test
    public void testGetAsListWithMissingKeyAndDefaultValueReturnsDefaultValue() throws Exception {
        XConfigList list = new XConfigList();
        assertEquals(list, object.getAsList("key2", list));
    }
}
