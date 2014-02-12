/**
 * XConfigListUnitTest.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig;

import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.*;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

/**
 * XConfigListUnitTest test class
 */
public class XConfigListUnitTest {

	private XConfigList object;

	@Before
	public void setUp() throws Exception {
		this.object = new XConfigList();
	}

	@Test
	public void testGetAsListReturnsExpectedList() throws Exception {
		List<XConfigValue> list = new ArrayList<XConfigValue>();
		list.add(new XConfigInteger(1));
		list.add(new XConfigBoolean(true));
		list.add(new XConfigFloat(1.2f));
		list.add(new XConfigList());
		list.add(new XConfigMap());
		list.add(new XConfigString("test"));
		for(XConfigValue node: list) {
			object.add(node);
		}
		Assert.assertEquals(list, object.getAsList());
	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsStringReturnsExpectedString() throws Exception {
		object.getAsString();
	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsFloatThrowsWrongTypeCastingException() throws Exception {
		object.getAsFloat();
	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsBooleanThrowsWrongTypeCastingException() throws Exception {
		object.getAsBoolean();
	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsMapThrowsWrongTypeCastingException() throws Exception {
		object.getAsMap();

	}

	@Test (expected = XConfigWrongTypeCastingException.class)
	public void testGetAsIntegerThrowsWrongTypeCastingException() throws Exception {
		object.getAsInteger();
	}
}
