package com.tuenti.xconfig.type;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import org.junit.Test;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;

public class XConfigListUnitTest {
	@Test
	public void testGetAsListReturnsExpectedObject() {
		XConfigList list = newXConfigList();

		assertEquals(list, list.getAsList());
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsStringReturnsExpectedString() {
		XConfigList list = newXConfigList();
		list.getAsString();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsFloatThrowsWrongTypeCastingException() {
		XConfigList list = newXConfigList();
		list.getAsFloat();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsBooleanThrowsWrongTypeCastingException() {
		XConfigList list = newXConfigList();
		list.getAsBoolean();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsMapThrowsWrongTypeCastingException() {
		XConfigList list = newXConfigList();
		list.getAsMap();

	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsIntegerThrowsWrongTypeCastingException() {
		XConfigList list = newXConfigList();
		list.getAsInteger();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsLongThrowsWrongTypeCastingException() {
		XConfigList list = newXConfigList();
		list.getAsLong();
	}

	@Test
	public void testContainsAllReturnsTrueWhenContainingAll() {
		XConfigList list = newXConfigList(
				new XConfigInteger(1),
				new XConfigBoolean(true),
				new XConfigFloat(1.2f),
				new XConfigLong(123L),
				new XConfigList(),
				new XConfigMap(),
				new XConfigString("test")
		);

		assertTrue(list.containsAll(asList(
				new XConfigInteger(1),
				new XConfigBoolean(true),
				new XConfigFloat(1.2f),
				new XConfigLong(123L),
				new XConfigList(),
				new XConfigMap(),
				new XConfigString("test")
		)));
	}

	@Test
	public void testContainsAllReturnsFalseWhenNotContainingAll() {
		XConfigList list = newXConfigList(
				new XConfigInteger(1),
				new XConfigBoolean(true)
		);

		assertFalse(list.containsAll(asList(
				new XConfigInteger(1),
				new XConfigBoolean(true),
				new XConfigString("non-existent")
		)));
	}

	@Test(expected = XConfigKeyNotFoundException.class)
	public void testGetThrowsExpectedKeyNotFoundException() {
		XConfigList list = newXConfigList(
				new XConfigInteger(1),
				new XConfigBoolean(true)
		);

		list.get(99);
	}

	@Test(expected = XConfigKeyNotFoundException.class)
	public void testGetWithNegativeIndexExpectedKeyNotFoundException() {
		XConfigList list = newXConfigList();

		list.get(-1);
	}

	@Test
	public void testGetReturnsExpectedValue() {
		XConfigList list = newXConfigList(
				new XConfigInteger(1),
				new XConfigBoolean(true),
				new XConfigFloat(1.2f),
				new XConfigLong(123L),
				new XConfigList(),
				new XConfigMap(),
				new XConfigString("test")
		);

		assertEquals(new XConfigInteger(1), list.get(0));
		assertEquals(new XConfigBoolean(true), list.get(1));
		assertEquals(new XConfigFloat(1.2f), list.get(2));
		assertEquals(new XConfigLong(123L), list.get(3));
		assertEquals(new XConfigList(), list.get(4));
		assertEquals(new XConfigMap(), list.get(5));
		assertEquals(new XConfigString("test"), list.get(6));
	}

	@Test
	public void testIndexOfReturnsCorrectIndex() {
		XConfigList list = newXConfigList(
				new XConfigInteger(1),
				new XConfigBoolean(true),
				new XConfigFloat(1.2f),
				new XConfigLong(123L),
				new XConfigList(),
				new XConfigMap(),
				new XConfigString("test")
		);

		assertEquals(0, list.indexOf(new XConfigInteger(1)));
		assertEquals(1, list.indexOf(new XConfigBoolean(true)));
		assertEquals(2, list.indexOf(new XConfigFloat(1.2f)));
		assertEquals(3, list.indexOf(new XConfigLong(123L)));
		assertEquals(4, list.indexOf(new XConfigList()));
		assertEquals(5, list.indexOf(new XConfigMap()));
		assertEquals(6, list.indexOf(new XConfigString("test")));
	}

	@Test
	public void testLastIndexOfReturnsCorrectIndex() {
		XConfigList list = newXConfigList(
				new XConfigInteger(1),
				new XConfigInteger(1),
				new XConfigBoolean(true),
				new XConfigBoolean(true),
				new XConfigBoolean(true),
				new XConfigFloat(1.2f)
		);

		assertEquals(1, list.lastIndexOf(new XConfigInteger(1)));
		assertEquals(4, list.lastIndexOf(new XConfigBoolean(true)));
		assertEquals(5, list.lastIndexOf(new XConfigFloat(1.2f)));
	}

	@Test
	public void testSubListReturnsExpectedSubList() {
		XConfigList list = newXConfigList(
				new XConfigInteger(1),
				new XConfigBoolean(true),
				new XConfigFloat(1.2f)
		);

		assertEquals(asList(
				new XConfigBoolean(true),
				new XConfigFloat(1.2f)
		), list.subList(1, 3));
	}

	@Test
	public void testSizeReturnsInternalsListSize() {
		XConfigList list = newXConfigList(
				new XConfigInteger(1),
				new XConfigBoolean(true),
				new XConfigFloat(1.2f)
		);

		assertEquals(3, list.size());
	}

	@Test
	public void testIsEmptyReturnsFalseWhenNotEmpty() {
		XConfigList list = newXConfigList(
				new XConfigInteger(1)
		);

		assertFalse(list.isEmpty());
	}

	@Test
	public void testIsEmptyReturnsTrueWhenEmpty() {
		XConfigList list = newXConfigList();

		assertTrue(list.isEmpty());
	}

	@Test
	public void testContainsReturnsTrueWhenContainingValue() {
		XConfigList list = newXConfigList(
				new XConfigInteger(1),
				new XConfigBoolean(true),
				new XConfigFloat(1.2f)
		);

		assertTrue(list.contains(new XConfigInteger(1)));
		assertTrue(list.contains(new XConfigBoolean(true)));
		assertTrue(list.contains(new XConfigFloat(1.2f)));
	}

	@Test
	public void testContainsReturnsFalseWhenNotContainingValue() {
		XConfigList list = newXConfigList(
				new XConfigInteger(1),
				new XConfigBoolean(true),
				new XConfigFloat(1.2f)
		);

		assertFalse(list.contains(new XConfigString("non-existent")));
	}

	@Test
	public void testToArrayReturnsExpectedArray() {
		XConfigList list = newXConfigList(
				new XConfigInteger(1),
				new XConfigBoolean(true),
				new XConfigFloat(1.2f)
		);

		assertArrayEquals(asList(
				new XConfigInteger(1),
				new XConfigBoolean(true),
				new XConfigFloat(1.2f)
		).toArray(), list.toArray());
	}

	@Test
	public void testGetAsJavaObject() {
		XConfigList list = newXConfigList(
				new XConfigInteger(1),
				new XConfigBoolean(true),
				new XConfigFloat(1.2f),
				new XConfigLong(123L),
				new XConfigList(),
				new XConfigMap(),
				new XConfigString("test")
		);

		List<Object> expectedList = asList(1, Boolean.TRUE, 1.2f, 123L,
				new LinkedList<>(), new HashMap<String, Object>(), "test");
		assertEquals(expectedList, list.getAsJavaObject());
	}

	private XConfigList newXConfigList(XConfigValue... items) {
		List<XConfigValue> backingList = asList(items);
		return XConfigList.wrapping(backingList);
	}
}
