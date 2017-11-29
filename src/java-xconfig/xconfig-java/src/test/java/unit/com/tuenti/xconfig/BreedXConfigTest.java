/**
 * BreedsXConfigTest.java
 *
 * Copyright (C) 2016 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig;

import static org.junit.Assert.assertEquals;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.type.XConfigMap;
import com.tuenti.xconfig.type.XConfigValue;

public class BreedXConfigTest {

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

	@Test
	public void testGetWithoutOverrides() {
		XConfigMap actualMap = getConfig("potatoConfig/details/a", "unknown", "unknown").getAsMap();
		XConfigMap expectedMap = xconfig.getAsMap("potatoConfig/details/a");
		assertEquals(expectedMap, actualMap);
	}

	@Test
	public void testGetWithOneOverride() {
		XConfigMap actualMap = getConfig("potatoConfig/details/a", "potatolandia", "unknown").getAsMap();
		XConfigMap expectedMap = xconfig.getAsMap("potatoTestResults/getWithOneOverride");
		assertEquals(expectedMap, actualMap);
	}

	@Test
	public void testGetWithTwoOverrides() {
		XConfigMap actualMap = getConfig("potatoConfig/details/a", "potatolandia", "potato-pre").getAsMap();
		XConfigMap expectedMap = xconfig.getAsMap("potatoTestResults/getWithTwoOverrides");
		assertEquals(expectedMap, actualMap);
	}

	@Test
	public void testGetWithTwoSimpleOverrides() {
		String actualName = getConfig("potatoConfig/names/a", "potatolandia", "potato-pre").getAsString();
		assertEquals("adriana", actualName);
	}

	@Test(expected = XConfigKeyNotFoundException.class)
	public void testMissingKeyThrowsException() {
		getConfig("potatoConfig/names/c", "potatolandia", "potato-pre").getAsString();
	}

	@Test
	public void testGetOverridedMissingKey() {
		String actualName = getConfig("potatoConfig/names/d", "potatolandia", "potato-pre").getAsString();
		assertEquals("dani", actualName);
	}

	@Test
	public void testGetBaseWithRecursiveOverrides() {
		XConfigMap actualMap = getConfig("potatoConfig", "potatolandia", "potato-pre").getAsMap();
		XConfigMap expectedMap = xconfig.getAsMap("potatoTestResults/getBaseWithRecursiveOverrides");
		assertEquals(expectedMap, actualMap);
	}

	private XConfigValue getConfig(String key, String country, String environment) {
		String breed[][] = {{"country", country}, {"environment", environment}};
		BreedXConfig breedXConfig = new BreedXConfig(xconfig, breed);
		return breedXConfig.getValue(key);
	}
}
