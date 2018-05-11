package com.tuenti.xconfig;

import static org.junit.Assert.*;

import org.junit.Test;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.testutils.XConfigBaseTestable;
import com.tuenti.xconfig.type.XConfigString;

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
	
	
	private XConfig xconfig(String ... keys) {
		return new XConfigBaseTestable(keys);
	}
	
	private BreedXConfig breededConfig(String ... keys) {
		return new BreedXConfig(xconfig(keys), BASE_BREEDS);
	}
}
