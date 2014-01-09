/**
 * XConfigIntegerUnitTest.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig;

import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigInteger;
import org.junit.*;

/**
 * XConfigIntegerUnitTest test class
 */
public class XConfigIntegerUnitTest {

    private XConfigInteger object;
    private Integer value;

    @Before
    public void setUp() throws Exception {
        this.value = 12345678;
        this.object = new XConfigInteger(this.value);
    }

    @Test
    public void testGetAsIntegerReturnsExpectedInteger() throws Exception {
        Assert.assertEquals(this.value, this.object.getAsInteger());
    }

    @Test
    public void testGetAsStringReturnsExpectedString() throws Exception {
        Assert.assertEquals(this.object.getAsString(), Integer.toString(this.value));
    }

    @Test
    public void testGetAsFloatReturnsExpectedFloat() throws Exception {
        Assert.assertEquals(this.object.getAsFloat(), new Float(this.value));
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
