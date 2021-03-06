/**
 * XConfigListUnitTest.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig.type;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;

/**
 * XConfigListUnitTest test class
 */
public class XConfigListUnitTest {

	private XConfigList object;
	private List<XConfigValue> internalList;

	@Before
	public void setUp() throws Exception {
		// Internal list
		internalList = new ArrayList<XConfigValue>();
		internalList.add(new XConfigInteger(1));
		internalList.add(new XConfigBoolean(true));
		internalList.add(new XConfigFloat(1.2f));
		internalList.add(new XConfigList());
		internalList.add(new XConfigMap());
		internalList.add(new XConfigString("test"));

		// Object under test from internal list
		this.object = new XConfigList(internalList);
	}

	@Test
	public void testGetAsListReturnsExpectedObject() throws Exception {
		assertEquals(object, object.getAsList());
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsStringReturnsExpectedString() throws Exception {
		object.getAsString();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsFloatThrowsWrongTypeCastingException() throws Exception {
		object.getAsFloat();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsBooleanThrowsWrongTypeCastingException() throws Exception {
		object.getAsBoolean();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsMapThrowsWrongTypeCastingException() throws Exception {
		object.getAsMap();

	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsIntegerThrowsWrongTypeCastingException() throws Exception {
		object.getAsInteger();
	}

	@Test
	public void testContainsAllReturnsTrueWhenContainingAll() {
		assertTrue(object.containsAll(internalList));
	}

	@Test
	public void testContainsAllReturnsFalseWhenNotContainingAll() {
		internalList.add(new XConfigString("non-existent"));
		assertFalse(object.containsAll(internalList));
	}

	@Test(expected = XConfigKeyNotFoundException.class)
	public void testGetThrowsExpectedKeyNotFoundException() throws Exception {
		object.get(internalList.size());
	}

	@Test(expected = XConfigKeyNotFoundException.class)
	public void testGetWithNegativeIndexExpectedKeyNotFoundException() throws Exception {
		object.get(-1);
	}

	@Test
	public void testGetReturnsExpectedValue() throws Exception {
		for (int i = 0; i < internalList.size(); i++) {
			assertEquals(internalList.get(i), object.get(i));
		}
	}

	@Test
	public void testIndexOfReturnsCorrectIndex() throws Exception {
		for (int i = 0; i < internalList.size(); i++) {
			assertEquals(i, object.indexOf(internalList.get(i)));
		}
	}

	@Test
	public void testLastIndexOfReturnsCorrectIndex() throws Exception {
		// Duplicate last element
		XConfigValue lastElement = internalList.get(internalList.size() - 1);
		internalList.add(lastElement);
		object.add(lastElement);

		// Check all lastIndexOf matches with internalList's
		for (int i = 0; i < internalList.size(); i++) {
			XConfigValue element = internalList.get(i);
			assertEquals(internalList.lastIndexOf(element), object.lastIndexOf(element));
		}

		// Check duplicated element returns last position
		assertEquals(internalList.size() - 1, object.lastIndexOf(lastElement));
	}

	@Test
	public void testSubListReturnsExpectedSubList() throws Exception {
		assertEquals(internalList.subList(internalList.size() / 2, internalList.size() - 1),
				object.subList(internalList.size() / 2, internalList.size() - 1));
	}

	@Test
	public void testSizeReturnsInternalsListSize() throws Exception {
		assertEquals(internalList.size(), object.size());
	}

	@Test
	public void testIsEmptyReturnsFalseWhenNotEmpty() throws Exception {
		assertFalse(object.isEmpty());
	}

	@Test
	public void testIsEmptyReturnsTrueWhenEmpty() throws Exception {
		object = new XConfigList();
		assertTrue(object.isEmpty());
	}

	@Test
	public void testContainsReturnsTrueWhenContainingValue() throws Exception {
		for (XConfigValue element : internalList) {
			assertTrue(object.contains(element));
		}
	}

	@Test
	public void testContainsReturnsFalseWhenNotContainingValue() throws Exception {
		assertFalse(object.contains(new XConfigString("non-existent")));
	}

	@Test
	public void testToArrayReturnsExpectedArray() throws Exception {
		assertArrayEquals(internalList.toArray(), object.toArray());
	}

	@Test
	public void testGetAsJavaObject() throws Exception {
		List<Object> expectedList = Arrays.asList((Object) new Integer(1), Boolean.TRUE, 1.2f,
				new LinkedList<Object>(), new HashMap<String, Object>(), "test");
		assertEquals(expectedList, this.object.getAsJavaObject());
	}
}
