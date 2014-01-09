/**
 * XConfigPathUnitTest.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.util.Arrays;
import java.util.Collection;

/**
 * XConfigPathUnitTest test class
 */
@RunWith(Parameterized.class)
public class XCConcatUnitTest {

    private final String[] inputString;
    private final String expectedOutput;

    public XCConcatUnitTest(final String expectedOutput, final String[] inputString) {
        this.inputString = inputString;
        this.expectedOutput = expectedOutput;
    }

    @Parameterized.Parameters
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {
                { "test/path", new String[] {"test", "path"} },
                { "test/value/path", new String[] {"test/value", "path"} },
                { "test", new String[] {"test"} },
                { "test#path", new String[] {"test#path"} },
                { "", new String[] {} }, // Empty array string case
        });
    }

    @Test
    public void testGetPathReturnsExpectedOutput() throws Exception {
        Assert.assertEquals(expectedOutput, XConfigPath.XCConcat((Object[])inputString));
    }
}
