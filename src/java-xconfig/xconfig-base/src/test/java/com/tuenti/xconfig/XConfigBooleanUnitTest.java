/**
 * XConfigBooleanUnitTest.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig;

import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigBoolean;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * XConfigBooleanUnitTest test class
 */
public class XConfigBooleanUnitTest {

    private XConfigBoolean object;
    private Boolean value;

    @Before
    public void setUp() throws Exception {
        this.value = true;
        this.object = new XConfigBoolean(this.value);
    }

    @Test
    public void testGetAsBooleanReturnsExpectedBoolean() throws Exception {
        Assert.assertEquals(this.value, this.object.getAsBoolean());
    }

    @Test
    public void testGetAsStringReturnsExpectedString() throws Exception {
        Assert.assertEquals(this.object.getAsString(), Boolean.toString(this.value));
    }

    @Test
    public void testGetAsFloatThrowsWrongTypeCastingException() throws Exception {
        try {
            this.object.getAsFloat();
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
