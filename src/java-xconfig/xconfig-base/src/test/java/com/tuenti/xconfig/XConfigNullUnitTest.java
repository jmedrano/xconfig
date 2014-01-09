/**
 * XConfigMapUnitTest.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.tuenti.xconfig.type.XConfigNull;

/**
 * XConfigMapUnitTest test class
 */
public class XConfigNullUnitTest {

    private XConfigNull object;

    @Before
    public void setUp() throws Exception {
        this.object = new XConfigNull();
    }

    @Test
    public void testGetAsListReturnsExpectedList() throws Exception {
        Assert.assertNull(this.object.getAsList());
    }

    @Test
    public void testGetAsStringReturnsExpectedString() throws Exception {
        Assert.assertNull(this.object.getAsString());
    }

    @Test
    public void testGetAsFloatReturnsExpectedFloat() throws Exception {
        Assert.assertNull(this.object.getAsFloat());
    }

    @Test
    public void testGetAsBooleanReturnsExpectedBoolean() throws Exception {
        Assert.assertNull(this.object.getAsBoolean());
    }

    @Test
    public void testGetAsIntegerReturnsExpectedBoolean() throws Exception {
        Assert.assertNull(this.object.getAsInteger());
    }
}
