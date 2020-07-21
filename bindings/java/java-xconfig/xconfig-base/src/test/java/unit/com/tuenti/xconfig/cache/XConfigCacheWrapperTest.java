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
public class XConfigCacheWrapperTest {

	@Mock
	private XConfig wrapped;
	@Mock
	private XConfigCacheProvider cacheProvider;
	@Mock
	private XConfigCache cache;

	private XConfigCacheWrapper wrapper;

	@Before
	public void setUp() throws Exception {
		when(cacheProvider.reloadCache(wrapped, null)).thenReturn(new XConfigCacheProvider.ReloadResult(true, cache));
		wrapper = new XConfigCacheWrapper(wrapped, cacheProvider);
		reset(cacheProvider);
	}

	@Test
	public void testUnwrapReturnsTheWrappedObject() {
		XConfig wrapped = wrapper.unwrap();
		assertSame(this.wrapped, wrapped);
	}

	@Test
	public void testCloseForwardsTheClose() {
		wrapper.close();
		verify(wrapped).close();
	}

	@Test
	public void testReloadCallsTheCacheProviderAndReturnsTheReloadedFlag() {
		when(cacheProvider.reloadCache(wrapped, cache)).thenReturn(new XConfigCacheProvider.ReloadResult(true, cache));

		boolean reloaded = wrapper.reload();

		assertTrue(reloaded);
		verify(cacheProvider).reloadCache(wrapped, cache);
	}

	@Test
	public void testGetValueSomeValue() {
		XConfigKeyNotFoundException notFoundException = new XConfigKeyNotFoundException("Hello World");
		when(cache.getValue("someKey", wrapped)).thenThrow(notFoundException);

		try {
			XConfigValue value = wrapper.getValue("someKey");
			fail();
		} catch (XConfigKeyNotFoundException e) {
			assertSame(notFoundException, e);
		}
	}

	@Test
	public void testGetValueNotFoundException() {
		XConfigKeyNotFoundException notFoundException = new XConfigKeyNotFoundException("Hello World");
		when(cache.getValue("someKey", wrapped)).thenThrow(notFoundException);

		try {
			XConfigValue value = wrapper.getValue("someKey");
			fail();
		} catch (XConfigKeyNotFoundException e) {
			assertSame(notFoundException, e);
		}
	}

	@Test
	public void testHasKeyExistingValue() {
		XConfigString someValue = new XConfigString("Hello World");
		when(cache.getValue("someKey", wrapped)).thenReturn(someValue);

		boolean hasKey = wrapper.hasKey("someKey");

		assertTrue(hasKey);
	}

	@Test
	public void testHasKeyNotFoundValue() {
		XConfigKeyNotFoundException notFoundException = new XConfigKeyNotFoundException("Hello World");
		when(cache.getValue("someKey", wrapped)).thenThrow(notFoundException);

		boolean hasKey = wrapper.hasKey("someKey");

		assertFalse(hasKey);
	}

	@Test
	public void testGetHashForwardsTheCall() {
		when(wrapped.getHash("someKey")).thenReturn(1234567L);

		long hash = wrapper.getHash("someKey");

		assertEquals(1234567L, hash);
		verify(wrapped).getHash("someKey");
	}
}