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
        Assert.assertEquals(value, object.getAsString());
    }

	@Test (expected = XConfigWrongTypeCastingException.class)
    public void testGetAsIntegerThrowsWrongTypeCastingException() throws Exception {
            object.getAsInteger();
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
    public void testGetAsListThrowsWrongTypeCastingException() throws Exception {
            object.getAsList();
    }
}
