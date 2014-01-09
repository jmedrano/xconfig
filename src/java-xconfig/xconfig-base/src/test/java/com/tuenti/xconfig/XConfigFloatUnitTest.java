/**
 * XConfigFloatUnitTest.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig;

import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigFloat;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * XConfigFloatUnitTest test class
 */
public class XConfigFloatUnitTest {

    private XConfigFloat object;
    private Float value;

    @Before
    public void setUp() throws Exception {
        this.value = 1.234f;
        this.object = new XConfigFloat(this.value);
    }

    @Test
    public void testGetAsFloatReturnsExpectedFloat() throws Exception {
        Assert.assertEquals(this.object.getAsFloat(), new Float(this.value));
    }

    @Test
    public void testGetAsStringReturnsExpectedString() throws Exception {
        Assert.assertEquals(this.object.getAsString(), Float.toString(this.value));
    }

    @Test
    public void testGetAsIntegerThrowsWrongTypeCastingException() throws Exception {
        try {
            this.object.getAsInteger();
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
    public void testGetAsMapThrowsWrongTypeCastingException() throws Exception {
        try {
            this.object.getAsMap();
            Assert.fail();
        } catch (XConfigWrongTypeCastingException e) {}
    }

    @Test
    public void testGetAsListThrowsWrongTypeCastingException() throws Exception {
        try {
            this.object.getAsList();
            Assert.fail();
        } catch (XConfigWrongTypeCastingException e) {}
    }
}
