/**
 * XConfigBreedsTest.java
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
		XConfigMap map1 = getConfig("potatoConfig/details/a", "unknown", "unknown").getAsMap();
		XConfigMap map2 = xconfig.getAsMap("potatoConfig/details/a");
		Assert.assertEquals(map1, map2);
	}

	@Test
	public void testGetWithOneOverride() throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		XConfigMap map1 = getConfig("potatoConfig/details/a", "potatolandia", "unknown").getAsMap();
		XConfigMap map2 = xconfig.getAsMap("potatoTestResults/getWithOneOverride");
		Assert.assertEquals(map1, map2);
	}

	@Test
	public void testGetWithTwoOverrides() throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		XConfigMap map1 = getConfig("potatoConfig/details/a", "potatolandia", "potato-pre").getAsMap();
		XConfigMap map2 = xconfig.getAsMap("potatoTestResults/getWithTwoOverrides");
		Assert.assertEquals(map1, map2);
	}

	@Test
	public void testGetWithTwoSimpleOverrides() throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		String name = getConfig("potatoConfig/names/a", "potatolandia", "potato-pre").getAsString();
		Assert.assertEquals("adriana", name);
	}

	@Test
	public void testMissingKeyThrowsException() throws Exception {
		try {
			getConfig("potatoConfig/names/c", "potatolandia", "potato-pre").getAsString();
			throw new Exception("A XConfigKeyNotFoundException exception was expected");
		} catch (XConfigKeyNotFoundException e) {
			// Exception was expected
		}
	}

	@Test
	public void testGetOverridedMissingKey() throws Exception {
		String name = getConfig("potatoConfig/names/d", "potatolandia", "potato-pre").getAsString();
		Assert.assertEquals("dani", name);
	}

	@Test
	public void testGetBaseWithRecursiveOverrides() throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		XConfigMap map1 = getConfig("potatoConfig", "potatolandia", "potato-pre").getAsMap();
		XConfigMap map2 = xconfig.getAsMap("potatoTestResults/getBaseWithRecursiveOverrides");
		Assert.assertEquals(map1, map2);
	}

	private XConfigValue getConfig(String key, String country, String environment) throws XConfigKeyNotFoundException {
		String breed[][] = {{"country", country}, {"environment", environment}};
		BreedXConfig breedXConfig = new BreedXConfig(xconfig, breed);
		return breedXConfig.getValue(key);
	}
}
