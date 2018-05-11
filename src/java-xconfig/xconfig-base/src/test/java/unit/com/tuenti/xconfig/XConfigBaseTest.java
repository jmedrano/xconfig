package com.tuenti.xconfig;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.tuenti.xconfig.testutils.XConfigBaseTestable;
import com.tuenti.xconfig.type.XConfigInteger;
import com.tuenti.xconfig.type.XConfigValueType;

public class XConfigBaseTest {
	private static final String VALID_KEY = "key";
	private static final String INVALID_KEY = "invalid key";
	private static final XConfigValueType VALID_TYPE = XConfigValueType.INTEGER;
	private static final XConfigValueType INVALID_TYPE = XConfigValueType.STRING;

	private XConfigBaseTestable xConfigBase;

	@Before
	public void setUp() {
		xConfigBase = new XConfigBaseTestable(VALID_KEY, new XConfigInteger(5));
	}

	@Test
	public void testHasValidKey() {
		assertFalse(xConfigBase.hasValidKey(INVALID_KEY, INVALID_TYPE));
		assertFalse(xConfigBase.hasValidKey(VALID_KEY, INVALID_TYPE));
		assertFalse(xConfigBase.hasValidKey(INVALID_KEY, VALID_TYPE));
		assertTrue(xConfigBase.hasValidKey(VALID_KEY, VALID_TYPE));
	}
}
