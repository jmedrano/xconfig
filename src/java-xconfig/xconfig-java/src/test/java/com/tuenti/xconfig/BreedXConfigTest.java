/**
 * BreedsXConfigTest.java
 *
 * Copyright (C) 2016 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigMap;
import com.tuenti.xconfig.type.XConfigValue;

/**
 * XConfigTest class
 */
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
	public void testGetWithoutOverrides() throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		XConfigMap actualMap = getConfig("potatoConfig/details/a", "unknown", "unknown").getAsMap();
		XConfigMap expectedMap = xconfig.getAsMap("potatoConfig/details/a");
		Assert.assertEquals(expectedMap, actualMap);
	}

	@Test
	public void testGetWithOneOverride() throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		XConfigMap actualMap = getConfig("potatoConfig/details/a", "potatolandia", "unknown").getAsMap();
		XConfigMap expectedMap = xconfig.getAsMap("potatoTestResults/getWithOneOverride");
		Assert.assertEquals(expectedMap, actualMap);
	}

	@Test
	public void testGetWithTwoOverrides() throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		XConfigMap actualMap = getConfig("potatoConfig/details/a", "potatolandia", "potato-pre").getAsMap();
		XConfigMap expectedMap = xconfig.getAsMap("potatoTestResults/getWithTwoOverrides");
		Assert.assertEquals(expectedMap, actualMap);
	}

	@Test
	public void testGetWithTwoSimpleOverrides() throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		String actualName = getConfig("potatoConfig/names/a", "potatolandia", "potato-pre").getAsString();
		Assert.assertEquals("adriana", actualName);
	}

	@Test(expected = XConfigKeyNotFoundException.class)
	public void testMissingKeyThrowsException() throws Exception {
		getConfig("potatoConfig/names/c", "potatolandia", "potato-pre").getAsString();
	}

	@Test
	public void testGetOverridedMissingKey() throws Exception {
		String actualName = getConfig("potatoConfig/names/d", "potatolandia", "potato-pre").getAsString();
		Assert.assertEquals("dani", actualName);
	}

	@Test
	public void testGetBaseWithRecursiveOverrides() throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		XConfigMap actualMap = getConfig("potatoConfig", "potatolandia", "potato-pre").getAsMap();
		XConfigMap expectedMap = xconfig.getAsMap("potatoTestResults/getBaseWithRecursiveOverrides");
		Assert.assertEquals(expectedMap, actualMap);
	}

	private XConfigValue getConfig(String key, String country, String environment) throws XConfigKeyNotFoundException {
		String breed[][] = {{"country", country}, {"environment", environment}};
		BreedXConfig breedXConfig = new BreedXConfig(xconfig, breed);
		return breedXConfig.getValue(key);
	}
}
