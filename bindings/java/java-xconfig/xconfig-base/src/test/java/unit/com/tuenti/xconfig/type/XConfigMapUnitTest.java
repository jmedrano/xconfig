package com.tuenti.xconfig.type;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.Test;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;

public class XConfigMapUnitTest {

	@Test
	public void testGetAsMapReturnsExpectedObject() {
		XConfigMap map = newXConfigMap();
		assertEquals(map, map.getAsMap());
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsStringThrowsWrongTypeCastingException() {
		newXConfigMap().getAsString();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsFloatThrowsWrongTypeCastingException() {
		newXConfigMap().getAsFloat();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsBooleanThrowsWrongTypeCastingException() {
		newXConfigMap().getAsBoolean();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsIntegerThrowsWrongTypeCastingException() {
		newXConfigMap().getAsInteger();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsLongThrowsWrongTypeCastingException() {
		newXConfigMap().getAsLong();
	}

	@Test(expected = XConfigWrongTypeCastingException.class)
	public void testGetAsListThrowsWrongTypeCastingException() {
		newXConfigMap().getAsList();
	}

	@Test
	public void testSizeReturnsExpectedSize() {
		XConfigMap map = newXConfigMap(
				"key1", new XConfigString("a-value"),
				"key2", new XConfigString("b-value")
		);
		assertEquals(2, map.size());
	}

	@Test
	public void testIsEmptyReturnsExpectedResult() {
		XConfigMap map = newXConfigMap(
				"key1", new XConfigString("a-value")
		);

		assertFalse(map.isEmpty());
	}

	@Test
	public void testContainsKeyReturnsTrueForExistingKey() {
		XConfigMap map = newXConfigMap(
				"key1", new XConfigString("a-value")
		);

		assertTrue(map.containsKey("key1"));
	}

	@Test
	public void testContainsKeyReturnsFalseForNonExistingKey() {
		XConfigMap map = newXConfigMap(
				"key1", new XConfigString("a-value")
		);

		assertFalse(map.containsKey("non-existent"));
	}

	@Test
	public void testContainsValueReturnsTrueForExistingKey() {
		XConfigMap map = newXConfigMap(
				"key1", new XConfigString("a-value"),
				"key2", new XConfigString("b-value")
		);

		assertTrue(map.containsValue(new XConfigString("a-value")));
	}

	@Test
	public void testContainsValueReturnsFalseForNonExistingKey() {
		XConfigMap map = newXConfigMap(
				"key1", new XConfigString("a-value")
		);

		assertFalse(map.containsValue(new XConfigInteger(2)));
	}

	@Test(expected = XConfigKeyNotFoundException.class)
	public void testGetThrowsExceptionWhenNotFound() {
		XConfigMap map = newXConfigMap();

		map.get("non-existent");
	}

	@Test
	public void testKeySetReturnsAllKeys() {
		XConfigMap map = newXConfigMap(
				"key1", new XConfigString("a-value"),
				"key2", new XConfigString("b-value")
		);

		assertEquals(new HashSet<>(asList("key1", "key2")), map.keySet());
	}

	@Test
	public void testValuesReturnsAllValues() {
		XConfigMap map = newXConfigMap(
				"key1", new XConfigString("a-value"),
				"key2", new XConfigString("b-value")
		);

		List<XConfigString> expectedValues = asList(new XConfigString("a-value"), new XConfigString("b-value"));
		Collection<XConfigValue> actualValues = map.values();
		assertTrue(actualValues.containsAll(expectedValues));
		assertTrue(expectedValues.containsAll(actualValues));
	}

	@Test
	public void testEntrySetReturnsAllEntriesSet() {
		XConfigMap map = newXConfigMap(
				"key1", new XConfigString("a-value"),
				"key2", new XConfigString("b-value")
		);

		Set<Map.Entry<String, XConfigValue>> entries = map.entrySet();
		List<String> keys = entries.stream().map(Map.Entry::getKey).collect(Collectors.toList());
		List<XConfigValue> values = entries.stream().map(Map.Entry::getValue).collect(Collectors.toList());
		assertEquals(asList("key1", "key2"), keys);
		assertEquals(asList(new XConfigString("a-value"), new XConfigString("b-value")), values);
	}

	@Test
	public void testGetAsJavaObject() {
		XConfigMap map = newXConfigMap(
				"key1", new XConfigInteger(1),
				"key2", new XConfigString("asd")
		);

		Map<String, Object> expectedMap = new HashMap<>();
		expectedMap.put("key1", 1);
		expectedMap.put("key2", "asd");
		assertEquals(expectedMap, map.getAsJavaObject());
	}

	@Test
	public void testGetAsStringWithKeyAndDefaultValueReturnsInternalValue() {
		XConfigMap map = newXConfigMap(
				"key1", new XConfigString("a-value")
		);

		assertEquals("a-value", map.getAsString("key1", null));
	}

	@Test
	public void testGetAsIntegerWithKeyAndDefaultValueReturnsInternalValue() {
		XConfigMap map = newXConfigMap(
				"key1", new XConfigInteger(1)
		);

		assertEquals((Integer) 1, map.getAsInteger("key1", null));
	}

	@Test
	public void testGetAsFloatWithKeyAndDefaultValueReturnsInternalValue() {
		XConfigMap map = newXConfigMap(
				"key1", new XConfigFloat(1)
		);

		assertEquals((Float) 1f, map.getAsFloat("key1", null));
	}

	@Test
	public void testGetAsLongWithKeyAndDefaultValueReturnsInternalValue() {
		XConfigMap map = newXConfigMap(
				"key1", new XConfigLong(1L)
		);

		assertEquals((Long) 1L, map.getAsLong("key1", null));
	}

	@Test
	public void testGetAsBooleanWithKeyAndDefaultValueReturnsInternalValue() {
		XConfigMap map = newXConfigMap(
				"key1", new XConfigBoolean(true)
		);

		assertEquals(true, map.getAsBoolean("key1", null));
	}

	@Test
	public void testGetAsMApWithKeyAndDefaultValueReturnsInternalValue() {
		XConfigMap map = newXConfigMap(
				"key1", newXConfigMap(
						"key2", new XConfigString("b-nested")
				)
		);

		assertEquals(newXConfigMap(
				"key2", new XConfigString("b-nested")
		), map.getAsMap("key1", null));
	}

	@Test
	public void testGetAsListWithKeyAndDefaultValueReturnsInternalValue() {
		XConfigMap map = newXConfigMap(
				"key1", new XConfigList()
		);

		assertEquals(new XConfigList(), map.getAsList("key1", null));
	}

	@Test
	public void testGetAsStringWithMissingKeyAndDefaultValueReturnsDefaultValue() {
		assertEquals("default", newXConfigMap().getAsString("key2", "default"));
	}

	@Test
	public void testGetAsIntegerWithMissingKeyAndDefaultValueReturnsDefaultValue() {
		assertEquals((Integer) 2, newXConfigMap().getAsInteger("key2", 2));
	}

	@Test
	public void testGetAsFloatWithMissingKeyAndDefaultValueReturnsDefaultValue() {
		assertEquals((Float) 2f, newXConfigMap().getAsFloat("key2", 2f));
	}

	@Test
	public void testGetAsLongWithMissingKeyAndDefaultValueReturnsDefaultValue() {
		assertEquals((Long) 2L, newXConfigMap().getAsLong("key2", 2L));
	}

	@Test
	public void testGetAsBooleanWithMissingKeyAndDefaultValueReturnsDefaultValue() {
		assertEquals(false, newXConfigMap().getAsBoolean("key2", false));
	}

	@Test
	public void testGetAsMapWithMissingKeyAndDefaultValueReturnsDefaultValue() {
		XConfigMap map = new XConfigMap();
		assertEquals(map, newXConfigMap().getAsMap("key2", map));
	}

	@Test
	public void testGetAsListWithMissingKeyAndDefaultValueReturnsDefaultValue() {
		XConfigList list = new XConfigList();
		assertEquals(list, newXConfigMap().getAsList("key2", list));
	}

	@Test
	public void testMergeWithNewKey() {
		XConfigMap mapBase = newXConfigMap("key1", new XConfigInteger(1));
		XConfigMap mapOverride = newXConfigMap("key2", new XConfigString("asd"));

		XConfigMap result = mapBase.mergeWith(mapOverride);

		assertEquals(new XConfigInteger(1), result.get("key1"));
		assertEquals(new XConfigString("asd"), result.get("key2"));
	}

	@Test
	public void testMergeWithExistentKey() {
		XConfigMap mapBase = newXConfigMap("key1", new XConfigInteger(1));
		XConfigMap mapOverride = newXConfigMap("key1", new XConfigInteger(27));

		XConfigMap result = mapBase.mergeWith(mapOverride);

		assertEquals(new XConfigInteger(27), result.get("key1"));
	}

	@Test
	public void testNestedMergeWith() {
		XConfigMap mapBase = newXConfigMap(
				"key1", new XConfigInteger(1),
				"parent", newXConfigMap(
						"key1", new XConfigString("eo"),
						"key2", new XConfigString("asd")
				)
		);
		XConfigMap mapOverride = newXConfigMap(
				"parent", newXConfigMap(
						"key2", new XConfigString("fyisudy")
				)
		);

		XConfigMap result = mapBase.mergeWith(mapOverride);

		assertEquals(new XConfigInteger(1), result.get("key1"));
		XConfigMap nestedMap = result.getAsMap("parent", null);
		assertNotNull(nestedMap);
		assertEquals(new XConfigString("eo"), nestedMap.get("key1"));
		assertEquals(new XConfigString("fyisudy"), nestedMap.get("key2"));
	}

	private XConfigMap newXConfigMap(Object... keyValueInPairs) {
		Map<String, XConfigValue> sourceMap = new HashMap<>();
		for (int i=0; i<keyValueInPairs.length-1; i+=2) {
			sourceMap.put(((String) keyValueInPairs[i]), ((XConfigValue) keyValueInPairs[i + 1]));
		}
		return XConfigMap.wrapping(sourceMap);
	}
}
