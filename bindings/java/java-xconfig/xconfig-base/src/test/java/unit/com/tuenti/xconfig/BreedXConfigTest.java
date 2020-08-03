package com.tuenti.xconfig;

import static org.junit.Assert.*;

import com.tuenti.xconfig.cache.XConfigCache;
import com.tuenti.xconfig.cache.XConfigCacheHook;
import com.tuenti.xconfig.cache.XConfigCacheProvider;
import com.tuenti.xconfig.cache.XConfigCacheWrapper;
import com.tuenti.xconfig.type.XConfigInteger;
import com.tuenti.xconfig.type.XConfigMap;
import com.tuenti.xconfig.type.XConfigValue;
import org.junit.Test;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.testutils.XConfigBaseTestable;
import com.tuenti.xconfig.type.XConfigString;

import java.util.HashMap;
import java.util.Map;

public class BreedXConfigTest {
	
	private final String[][] BREEDS = new String[][] {{"breed1", "breed1Value"}, {"breed2", "breed2Value"}};
	private final String[][] BASE_BREEDS = new String[][] {{"breed0", "breed0Value"}};

	@Test
	public void testHasKeyForNormalBaseXConfigAndNotBreededValue() {
		XConfig base = xconfig(
			"root/key", "value"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		assertTrue(breedConfig.hasKey("root/key"));
	}
	
	@Test
	public void testHasKeyForNormalBaseXConfigAndBreededValue() {
		XConfig base = xconfig(
			"root_breeds/breed1/breed1Value/key", "value"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		assertTrue(breedConfig.hasKey("root/key"));
	}
	
	@Test
	public void testHasKeyForNormalBaseXConfigAndMissingValue() {
		XConfig base = xconfig(
			"root/key", "value",
			"root_breeds/breed1/breed1Value/key", "value"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		assertFalse(breedConfig.hasKey("root/key2"));
	}
	
	@Test
	public void testHasKeyForBreededBaseXConfigAndNotBreededValue() {
		XConfig base = breededConfig(
			"root/key", "value"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		assertTrue(breedConfig.hasKey("root/key"));
	}
	
	@Test
	public void testHasKeyForBreededBaseXConfigAndBreededValue() {
		XConfig base = breededConfig(
			"root_breeds/breed1/breed1Value/key", "value"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		assertTrue(breedConfig.hasKey("root/key"));
	}
	
	@Test
	public void testHasKeyForBreededBaseXConfigAndBreededValueInBase() {
		XConfig base = breededConfig(
			"root_breeds/breed0/breed0Value/key", "value"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		assertTrue(breedConfig.hasKey("root/key"));
	}
	
	@Test
	public void testHasKeyForBreededBaseXConfigAndCombinedBreededValue() {
		XConfig base = breededConfig(
			"root_breeds/breed1Breed0/breed1Value_breed0Value/key", "value"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		assertTrue(breedConfig.hasKey("root/key"));
	}
	
	@Test
	public void testHasKeyForBreededBaseXConfigAndWronglyOrderedCombinedBreededValue() {
		XConfig base = breededConfig(
			"root_breeds/breed0Breed1/breed0Value_breed1Value/key", "value"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		assertFalse(breedConfig.hasKey("root/key"));
	}
	
	@Test
	public void testHasKeyForBreededBaseXConfigAndMissingValue() {
		XConfig base = breededConfig(
			"root/key", "value",
			"root_breeds/breed1/breed1Value/key", "value"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		assertFalse(breedConfig.hasKey("root/key2"));
	}
	
	@Test
	public void testGetValueForNormalBaseXConfigAndNotBreededValue() {
		XConfig base = xconfig(
			"root/key", "value",
			"root_breeds/breed1/breed1Value/key2", "value_bad"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		assertEquals(new XConfigString("value"), breedConfig.getValue("root/key"));
	}
	
	@Test
	public void testGetValueForNormalBaseXConfigAndBreededValue() {
		XConfig base = xconfig(
			"root/key", "value_bad",
			"root_breeds/breed1/breed1Value/key", "value"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		assertEquals(new XConfigString("value"), breedConfig.getValue("root/key"));
	}
	
	@Test
	public void testGetValueForNormalBaseXConfigAndDoubleBreededValue() {
		XConfig base = xconfig(
			"root/key", "value_bad",
			"root_breeds/breed1/breed1Value/key", "value_bad_2",
			"root_breeds/breed2/breed2Value/key", "value"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		assertEquals(new XConfigString("value"), breedConfig.getValue("root/key"));
	}
	
	@Test(expected=XConfigKeyNotFoundException.class)
	public void testGetValueForNormalBaseXConfigAndMissingValue() {
		XConfig base = xconfig(
			"root/key", "value",
			"root_breeds/breed1/breed1Value/key", "value"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		breedConfig.getValue("root/key2");
	}

	@Test
	public void testGetValueForBreededBaseXConfigAndNotBreededValue() {
		XConfig base = breededConfig(
			"root/key", "value",
			"root_breeds/breed1/breed1Value/key2", "value_bad"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		assertEquals(new XConfigString("value"), breedConfig.getValue("root/key"));
	}
	
	@Test
	public void testGetValueForBreededBaseXConfigAndBreededValue() {
		XConfig base = breededConfig(
			"root/key", "value_bad",
			"root_breeds/breed0/breed0Value/key", "value",
			"root_breeds/breed1/breed1Value/key", "value"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		assertEquals(new XConfigString("value"), breedConfig.getValue("root/key"));
	}
	
	@Test
	public void testGetValueForBreededBaseXConfigAndDoubleBreededValue() {
		XConfig base = breededConfig(
			"root/key", "value_bad",
			"root_breeds/breed0/breed0Value/key", "value_bad_3",
			"root_breeds/breed1/breed1Value/key", "value_bad_2",
			"root_breeds/breed2/breed2Value/key", "value"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		assertEquals(new XConfigString("value"), breedConfig.getValue("root/key"));
	}
	
	@Test
	public void testGetValueForBreededBaseXConfigAndBreededValueInBase() {
		XConfig base = breededConfig(
			"root/key", "value_bad",
			"root_breeds/breed1/breed1Value/key2", "value_bad_2",
			"root_breeds/breed0/breed0Value/key", "value"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		assertEquals(new XConfigString("value"), breedConfig.getValue("root/key"));
	}
	
	@Test
	public void testGetValueForBreededBaseXConfigAndDoubleBreededValueIncludingBase() {
		XConfig base = breededConfig(
			"root/key", "value_bad",
			"root_breeds/breed1/breed1Value/key", "value",
			"root_breeds/breed0/breed0Value/key", "value_bad_2"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		assertEquals(new XConfigString("value"), breedConfig.getValue("root/key"));
	}
	
	@Test
	public void testGetValueForBreededBaseXConfigAndCombinedBreededValue() {
		XConfig base = breededConfig(
			"root/key", "value_bad",
			"root_breeds/breed1/breed1Value/key", "value_bad_3",
			"root_breeds/breed0/breed0Value/key", "value_bad_2",
			"root_breeds/breed1Breed0/breed1Value_breed0Value/key", "value"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		assertEquals(new XConfigString("value"), breedConfig.getValue("root/key"));
	}
	
	@Test
	public void testGetValueForBreededBaseXConfigAndWronglyOrderedCombinedBreededValue() {
		XConfig base = breededConfig(
			"root/key", "value_bad",
			"root_breeds/breed1/breed1Value/key", "value",
			"root_breeds/breed0/breed0Value/key", "value_bad_2",
			"root_breeds/breed0Breed1/breed0Value_breed1Value/key", "value_bad_3"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		assertEquals(new XConfigString("value"), breedConfig.getValue("root/key"));
	}

	@Test(expected=XConfigKeyNotFoundException.class)
	public void testGetValueForBreededBaseXConfigAndMissingValue() {
		XConfig base = breededConfig(
			"root/key", "value",
			"root_breeds/breed1/breed1Value/key", "value",
			"root_breeds/breed0/breed0Value/key", "value"
		);
		BreedXConfig breedConfig = new BreedXConfig(base, BREEDS);
		breedConfig.getValue("root/key2");
	}

	@Test
	public void testGetValueBreededFromScalarToMap() {
		XConfigBaseTestable base = new XConfigBaseTestable()
				.addKey("root/key", newXConfigMap("a", "a-value"))
				.addKey("root_breeds/breed0/breed0Value/key", new XConfigInteger(9));
		BreedXConfig breedXConfig = new BreedXConfig(base, BASE_BREEDS);

		XConfigValue value = breedXConfig.getValue("root/key");
		assertEquals(new XConfigInteger(9), value);
	}

	@Test
	public void testGetValueBreededFromMapToScalar() {
		XConfigBaseTestable base = new XConfigBaseTestable()
				.addKey("root/key", new XConfigInteger(9))
				.addKey("root_breeds/breed0/breed0Value/key", newXConfigMap("a", "a-value"));
		BreedXConfig breedXConfig = new BreedXConfig(base, BASE_BREEDS);

		XConfigValue value = breedXConfig.getValue("root/key");
		assertEquals(newXConfigMap("a", "a-value"), value);
	}

	/** Test for bug #FRWK-532 */
	@Test
	public void testCachedXConfigNotAffectedByBreedOverride() {
		XConfigBaseTestable xconfig = new XConfigBaseTestable()
				.addKey("base/mapValue", newXConfigMap(
						"a", "a-value",
						"b", "b-value"
				)).addKey("base_breeds/breed0/breed0Value/mapValue", newXConfigMap(
						"a", "overridden-a",
						"c", "c-value"
				));
		XConfig cachedXConfig = cacheXConfig(xconfig);
		BreedXConfig breedXConfig = new BreedXConfig(cachedXConfig, BASE_BREEDS);

		XConfigMap baseMap = cachedXConfig.getAsMap("base/mapValue");
		XConfigMap overriddenMap = breedXConfig.getAsMap("base/mapValue");
		XConfigMap baseMapReread = cachedXConfig.getAsMap("base/mapValue");
		XConfigMap overriddenMapReread = breedXConfig.getAsMap("base/mapValue");

		XConfigMap expectedBaseMap = newXConfigMap(
				"a", "a-value",
				"b", "b-value"
		);
		assertEquals(expectedBaseMap, baseMap);
		assertEquals(expectedBaseMap, baseMapReread);

		XConfigMap expectedOverriddenMap = newXConfigMap(
				"a", "overridden-a",
				"b", "b-value",
				"c", "c-value"
		);
		assertEquals(expectedOverriddenMap, overriddenMap);
		assertEquals(expectedOverriddenMap, overriddenMapReread);
	}

	private XConfig xconfig(String ... keys) {
		return new XConfigBaseTestable(keys);
	}

	private BreedXConfig breededConfig(String ... keys) {
		return new BreedXConfig(xconfig(keys), BASE_BREEDS);
	}

	private XConfigMap newXConfigMap(String... keyValueInPairs) {
		Map<String, XConfigValue> sourceMap = new HashMap<>();
		for (int i=0; i<keyValueInPairs.length-1; i+=2) {
			sourceMap.put(keyValueInPairs[i], new XConfigString(keyValueInPairs[i + 1]));
		}
		return new XConfigMap(sourceMap);
	}

	private XConfig cacheXConfig(XConfig base) {
		return new XConfigCacheWrapper(base, (config, currentCache) -> {
			if (currentCache == null) {
				XConfigCache xConfigCache = new XConfigCache(0, new XConfigCacheHook() {});
				return new XConfigCacheProvider.ReloadResult(true, xConfigCache);
			} else {
				return new XConfigCacheProvider.ReloadResult(false, currentCache);
			}
		});
	}
}
