/**
 * XConfigStringUnitTest.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig;

import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigString;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * XConfigStringUnitTest test class
 */
public class XConfigStringUnitTest {

    private XConfigString object;
    private String value;

    @Before
    public void setUp() throws Exception {
        this.value = "test_string";
        this.object = new XConfigString(this.value);
    }

    @Test
    public void testGetAsStringReturnsExpectedString() throws Exception {
        Assert.assertEquals(this.value, this.object.getAsString());
    }

    @Test
    public void testGetAsIntegerThrowsWrongTypeCastingException() throws Exception {
        try {
            this.object.getAsInteger();
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
