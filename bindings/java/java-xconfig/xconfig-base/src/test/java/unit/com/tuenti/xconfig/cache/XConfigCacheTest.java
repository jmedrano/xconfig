package com.tuenti.xconfig.cache;

import com.tuenti.xconfig.XConfig;
import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.type.XConfigString;
import com.tuenti.xconfig.type.XConfigValue;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

@RunWith(MockitoJUnitRunner.class)
public class XConfigCacheTest {
	private static final long CONFIG_HASH = 1234567L;

	@Mock
	private XConfigCacheHook hook;
	@Mock
	private XConfig provider;

	private XConfigCache cache;

	@Before
	public void setUp() {
		cache = new XConfigCache(CONFIG_HASH, hook);
	}

	@Test
	public void testIsCacheValidForCurrentConfigForCurrentConfigHash() {
		assertTrue(cache.isCacheValidForCurrentConfig(CONFIG_HASH));
	}

	@Test
	public void testIsCacheValidForCurrentConfigForOtherConfigHash() {
		assertFalse(cache.isCacheValidForCurrentConfig(7654321L));
	}

	@Test
	public void testCacheLoadingAnyValue() {
		XConfigString expectedValue = new XConfigString("Hello World");
		when(provider.getValue("testCacheLoadingAnyValue")).thenReturn(expectedValue);

		XConfigValue value = cache.getValue("testCacheLoadingAnyValue", provider);

		assertSame(expectedValue, value);
		verify(hook).onSetValue("testCacheLoadingAnyValue", expectedValue);
	}

	@Test
	public void testCacheLoadingNotFoundValue() {
		XConfigKeyNotFoundException notFoundException = new XConfigKeyNotFoundException("not found");
		when(provider.getValue("testCacheLoadingNotFoundValue")).thenThrow(notFoundException);

		try {
			XConfigValue value = cache.getValue("testCacheLoadingNotFoundValue", provider);
			fail();
		} catch (XConfigKeyNotFoundException e) {
			assertSame(notFoundException, e);
			verify(hook).onSetValueNotFound("testCacheLoadingNotFoundValue");
		}
	}
}