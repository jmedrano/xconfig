package com.tuenti.xconfig.parser;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.FileNotFoundException;

import org.junit.Before;
import org.junit.Test;

import com.tuenti.xconfig.parser.ConfigParser;
import com.tuenti.xconfig.type.XConfigInteger;
import com.tuenti.xconfig.type.XConfigList;
import com.tuenti.xconfig.type.XConfigString;

public class MergeTest {
	private ConfigParser configParser;

	@Before
	public void setup() {
		configParser = new ConfigParser();
	}

	@Test
	public void testMerge() throws FileNotFoundException {
		configParser.addFile(new File("src/test/resources/merge1/dbConf.yaml"));
		configParser.addFile(new File("src/test/resources/merge2/dbConf.yaml"));
		XConfigList expectedList = new XConfigList();
		expectedList.add(new XConfigInteger(1));

		assertEquals(new XConfigInteger(1), configParser.getElement("dbConfig/scalarEntry"));
		assertEquals(new XConfigString("val1"), configParser.getElement("dbConfig/mapEntry/key1"));
		assertEquals(new XConfigString("otherval2"),
				configParser.getElement("dbConfig/mapEntry/key2"));
		assertEquals(new XConfigString("otherval3"),
				configParser.getElement("dbConfig/mapEntry/key3"));
		assertEquals(expectedList, configParser.getElement("dbConfig/listEntry"));
		assertEquals(new XConfigString("val1"),
				configParser.getElement("dbConfig/scalarEntry2/key1"));
		assertEquals(new XConfigString("val2"),
				configParser.getElement("dbConfig/scalarEntry2/key2"));
		
		assertEquals(new XConfigString("leaf1"), configParser.getElement("tree/branch/branch/branch/leaf1"));
		assertEquals(new XConfigString("leaf2"), configParser.getElement("tree/branch/branch/branch/leaf2"));
		assertEquals(new XConfigString("leaf3"), configParser.getElement("tree/branch/leaf3"));
		assertEquals(new XConfigString("leaf4"), configParser.getElement("tree/branch/branch/leaf4"));

	}
}
