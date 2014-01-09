/**
 * XConfigMapUnitTest.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig;

import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigInteger;
import com.tuenti.xconfig.type.XConfigMap;
import com.tuenti.xconfig.type.XConfigValue;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Map;

/**
 * XConfigMapUnitTest test class
 */
public class XConfigMapUnitTest {

    private XConfigMap object;

    @Before
    public void setUp() throws Exception {
        this.object = new XConfigMap();
    }

    @Test
    public void testGetAsListReturnsExpectedList() throws Exception {
        this.object.add("key1", new XConfigInteger(1));
        Map<String, XConfigValue> map = this.object.getAsMap();
        Assert.assertTrue(map.containsKey("key1"));
        XConfigValue value = map.get("key1");
        Assert.assertEquals(1, value.getAsInteger().intValue());
    }

    @Test
    public void testGetAsStringThrowsWrongTypeCastingException() throws Exception {
        try {
            this.object.getAsString();
            Assert.fail();
        } catch (XConfigWrongTypeCastingException e) {}
    }

    @Test
    public void testGetAsFloatThrowsWrongTypeCastingException() throws Exception {
        try {
            this.object.getAsFloat();
            Assert.fail();
        } catch (XConfigWrongTypeCastingException e) {}
    }

    @Test
    public void testGetAsBooleanThrowsWrongTypeCastingException() throws Exception {
        try {
            this.object.getAsBoolean();
            Assert.fail();
        } catch (XConfigWrongTypeCastingException e) {}
    }

    @Test
    public void testGetAsIntegerThrowsWrongTypeCastingException() throws Exception {
        try {
            this.object.getAsInteger();
            Assert.fail();
        } catch (XConfigWrongTypeCastingException e) {}
    }
}
