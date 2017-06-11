package com.tuenti.xconfig;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.type.XConfigInteger;
import com.tuenti.xconfig.type.XConfigValue;
import com.tuenti.xconfig.type.XConfigValueType;

public class XConfigBaseTest {
	private static final String VALID_KEY = "key";
	private static final String INVALID_KEY = "invalid key";
	private static final XConfigValueType VALID_TYPE = XConfigValueType.INTEGER;
	private static final XConfigValueType INVALID_TYPE = XConfigValueType.STRING;

	private XConfigBaseTestable xConfigBase;

	@Before
	public void setUp() {
		xConfigBase = new XConfigBaseTestable();
	}

	@Test
	public void testHasValidKey() {
		assertFalse(xConfigBase.hasValidKey(INVALID_KEY, INVALID_TYPE));
		assertFalse(xConfigBase.hasValidKey(VALID_KEY, INVALID_TYPE));
		assertFalse(xConfigBase.hasValidKey(INVALID_KEY, VALID_TYPE));
		assertTrue(xConfigBase.hasValidKey(VALID_KEY, VALID_TYPE));
	}

	static class XConfigBaseTestable extends XConfigBase {

		@Override
		public void close() {}

		@Override
		public boolean reload() {
			return false;
		}

		@Override
		public XConfigValue getValue(String key) throws XConfigKeyNotFoundException {
			return new XConfigInteger(5);
		}

		@Override
		public boolean hasKey(String key) {
			return VALID_KEY.equals(key);
		}

		@Override
		public long getLastModificationTime(String key) throws XConfigKeyNotFoundException {
			return 0;
		}
	}
}
