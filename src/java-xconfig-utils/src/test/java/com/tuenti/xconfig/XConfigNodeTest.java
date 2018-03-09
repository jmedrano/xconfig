package com.tuenti.xconfig;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import org.junit.Test;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.tuenti.xconfig.parser.YamlXConfig;

public class XConfigNodeTest {

	private static final String ROOT_NODE = "rootNode";
	private static final String[][] BREEDS_ARRAY = {{"breedKey", "breedValue"}};
	private static final String[][] OTHER_BREEDS_ARRAY = {{"breedKey2", "breedValue2"}};
	private static final Map<String, String> BREEDS_MAP = Collections.singletonMap("breedKey", "breedValue");
	private static final Map<String, String> OTHER_BREEDS_MAP = Collections.singletonMap("breedKey2", "breedValue2");

	@Test
	public void testIntegerFromString() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("\"1234\""));

		int result = xConfigNode.getInteger("subnode", "value").get();

		assertEquals(1234, result);
	}

	@Test
	public void testIntegerAsString() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("1234"));

		String result = xConfigNode.getString("subnode", "value").get();

		assertEquals("1234", result);
	}

	@Test
	public void testIntegerFromInteger() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("1234"));

		int result = xConfigNode.getInteger("subnode", "value").get();

		assertEquals(1234, result);
	}

	@Test
	public void testInvalidInteger() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("invalid_integer"));

		Optional<Integer> result = xConfigNode.getInteger("subnode", "value");

		assertEquals(Optional.empty(), result);
	}

	@Test
	public void testMissingInteger() throws Exception {
		XConfigNode xConfigNode = createEmptyXConfig();

		Optional<Integer> result = xConfigNode.getInteger("subnode", "value");

		assertEquals(Optional.empty(), result);
	}

	@Test
	public void testMissingLeafInteger() throws Exception {
		XConfigNode xConfigNode = createEmptyXConfig();

		Optional<Integer> result = xConfigNode.getInteger("value");

		assertEquals(Optional.empty(), result);
	}

	@Test
	public void testLongFromString() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("\"1234\""));

		long result = xConfigNode.getLong("subnode", "value").get();

		assertEquals(1234, result);
	}

	@Test
	public void testLongFromLong() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("1234"));

		long result = xConfigNode.getLong("subnode", "value").get();

		assertEquals(1234, result);
	}

	@Test
	public void testInvalidLong() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("invalid_long"));

		Optional<Long> result = xConfigNode.getLong("subnode", "value");

		assertEquals(Optional.empty(), result);
	}

	@Test
	public void testString() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("\"any string\""));

		String result = xConfigNode.getString("subnode", "value").get();

		assertEquals("any string", result);
	}

	@Test
	public void testMissingString() throws Exception {
		XConfigNode xConfigNode = createEmptyXConfig();

		Optional<String> result = xConfigNode.getString("subnode", "value");

		assertEquals(Optional.empty(), result);
	}

	@Test
	public void testMissingLeafString() throws Exception {
		XConfigNode xConfigNode = createEmptyXConfig();

		Optional<String> result = xConfigNode.getString("value");

		assertEquals(Optional.empty(), result);
	}

	@Test
	public void testFalseBoolean() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("false"));

		boolean result = xConfigNode.getBoolean("subnode", "value").get();

		assertEquals(false, result);
	}

	@Test
	public void testFalseBooleanFromString() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("\"false\""));

		boolean result = xConfigNode.getBoolean("subnode", "value").get();

		assertEquals(false, result);
	}

	@Test
	public void testTrueBoolean() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("\"true\""));

		boolean result = xConfigNode.getBoolean("subnode", "value").get();

		assertEquals(true, result);
	}

	@Test
	public void testTrueBooleanFromString() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("true"));

		boolean result = xConfigNode.getBoolean("subnode", "value").get();

		assertEquals(true, result);
	}

	@Test
	public void testInvalidBoolean() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("invalid_boolean"));

		Optional<Boolean> result = xConfigNode.getBoolean("subnode", "value");

		assertEquals(Optional.empty(), result);
	}

	@Test
	public void testBigDecimalFromString() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("\"12.1\""));

		BigDecimal result = xConfigNode.getBigDecimal("subnode", "value").get();

		assertEquals(new BigDecimal("12.1"), result);
	}

	@Test
	public void testFloatFromFloat() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("12.1"));

		BigDecimal result = xConfigNode.getBigDecimal("subnode", "value").get();

		assertEquals(new BigDecimal("12.1"), result);
	}

	@Test
	public void testInvalidFloat() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("invalid_float"));

		Optional<BigDecimal> result = xConfigNode.getBigDecimal("subnode", "value");

		assertEquals(Optional.empty(), result);
	}

	@Test
	public void testMissingFloat() throws Exception {
		XConfigNode xConfigNode = createEmptyXConfig();

		Optional<BigDecimal> result = xConfigNode.getBigDecimal("subnode", "value");

		assertEquals(Optional.empty(), result);
	}

	@Test
	public void testMissingLeafFloat() throws Exception {
		XConfigNode xConfigNode = createEmptyXConfig();

		Optional<BigDecimal> result = xConfigNode.getBigDecimal("value");

		assertEquals(Optional.empty(), result);
	}

	@Test
	public void testSubnodeWithValidValue() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("{\"integer\":123}"));

		XConfigNode result = xConfigNode.getSubNode("subnode", "value");

		assertEquals(Optional.of(123), result.getInteger("integer"));
	}

	@Test
	public void testSubnodeWithMissingBranch() throws Exception {
		XConfigNode xConfigNode = createEmptyXConfig();

		XConfigNode result = xConfigNode.getSubNode("subnode", "value");

		assertEquals(Optional.empty(), result.getInteger("integer"));
	}

	@Test
	public void testSubnodeWithValidComposedDescendants() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("{\"integer\":123}"));

		XConfigNode result = xConfigNode.getSubNode("subnode");

		assertEquals(Optional.of(123), result.getInteger("value", "integer"));
	}

	@Test
	public void testSubnodeWithMissingDescendant() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("{\"integer\":123}"));

		XConfigNode result = xConfigNode.getSubNode("subnode");

		assertEquals(Optional.empty(), result.getInteger("invalid_branch"));
	}

	@Test
	public void testIntegerStream() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("[1, 2, 3]"));

		Stream<Integer> result = xConfigNode.getIntegerStream("subnode", "value");

		assertArrayEquals(Stream.of(1, 2, 3).toArray(), result.toArray());
	}

	@Test
	public void testIntegerStreamFromAListContainingNonIntegerValues() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("[1, two, 3]"));

		Stream<Integer> result = xConfigNode.getIntegerStream("subnode", "value");

		assertArrayEquals(Stream.of(1, 3).toArray(), result.toArray());
	}

	@Test
	public void testInvalidStream() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("invalid_list"));

		Stream<Integer> result = xConfigNode.getIntegerStream("subnode", "value");

		assertEquals(0, result.count());
	}

	@Test
	public void testLongStream() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("[1, 2, 3]"));

		Stream<Long> result = xConfigNode.getLongStream("subnode", "value");

		assertArrayEquals(Stream.of(1L, 2L, 3L).toArray(), result.toArray());
	}

	@Test
	public void testLongStreamFromAListContainingNonNumericValues() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("[1, two, 3]"));

		Stream<Long> result = xConfigNode.getLongStream("subnode", "value");

		assertArrayEquals(Stream.of(1L, 3L).toArray(), result.toArray());
	}

	@Test
	public void testInvalidLongStream() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("invalid_list"));

		Stream<Long> result = xConfigNode.getLongStream("subnode", "value");

		assertEquals(0, result.count());
	}

	@Test
	public void testBigDecimalStream() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("[1.1, 2.1, 3.1]"));

		Stream<BigDecimal> result = xConfigNode.getBigDecimalStream("subnode", "value");

		Stream<BigDecimal> expectedStream = Stream.of(
				new BigDecimal("1.1"), new BigDecimal("2.1"), new BigDecimal("3.1"));
		assertArrayEquals(expectedStream.toArray(), result.toArray());
	}

	@Test
	public void testBigDecimalStreamFromListWithNonDecimalValues() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("[1.1, 2.1, 3.1, non_float]"));

		Stream<BigDecimal> result = xConfigNode.getBigDecimalStream("subnode", "value");

		Stream<BigDecimal> expectedStream = Stream.of(
				new BigDecimal("1.1"), new BigDecimal("2.1"), new BigDecimal("3.1"));
		assertArrayEquals(expectedStream.toArray(), result.toArray());
	}

	@Test
	public void testStringStream() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("[one, two, three, caramba]"));

		Stream<String> result = xConfigNode.getStringStream("subnode", "value");

		assertArrayEquals(Stream.of("one", "two", "three", "caramba").toArray(), result.toArray());
	}

	@Test
	public void testStringStreamWithNoStringValue() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("[one, two, three, {\"a\":\"map\"}]"));

		Stream<String> result = xConfigNode.getStringStream("subnode", "value");

		assertArrayEquals(Stream.of("one", "two", "three").toArray(), result.toArray());
	}

	@Test
	public void testMapOfSubnodes() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("{\"map1\":{\"key\":1},\"map2\":{\"key\":2}}"));
		Map<String, XConfigNode> result = xConfigNode.getMapOfNodes("subnode", "value");

		assertEquals(1, result.get("map1").getInteger("key").get().intValue());
		assertEquals(2, result.get("map2").getInteger("key").get().intValue());
	}

	@Test
	public void testMapOfSubnodesReturnsEmptyMapIfNotAValidMap() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("non_map_value"));
		Map<String, XConfigNode> result = xConfigNode.getMapOfNodes("subnode", "value");

		assertTrue(result.isEmpty());
	}

	@Test
	public void testRequiredIntegerFromNumericString() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("\"1234\""));

		int result = xConfigNode.getRequiredInteger("subnode", "value");

		assertEquals(1234, result);
	}

	@Test
	public void testRequiredIntegerFromInteger() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("1234"));

		int result = xConfigNode.getRequiredInteger("subnode", "value");

		assertEquals(1234, result);
	}

	@Test(expected=InvalidConfigException.class)
	public void testRequiredInvalidInteger() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("invalid_integer"));

		xConfigNode.getRequiredInteger("subnode", "value");
	}

	@Test(expected=InvalidConfigException.class)
	public void testRequiredIntegerFromMissingValue() throws Exception {
		XConfigNode xConfigNode = createEmptyXConfig();

		xConfigNode.getRequiredInteger("subnode", "value");
	}

	@Test
	public void testRequiredLongFromNumericValue() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("\"1234\""));

		long result = xConfigNode.getRequiredLong("subnode", "value");

		assertEquals(1234, result);
	}

	@Test(expected=InvalidConfigException.class)
	public void testRequiredInvalidLong() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("invalid_integer"));

		xConfigNode.getRequiredLong("subnode", "value");
	}

	@Test(expected=InvalidConfigException.class)
	public void testRequiredLongFromMissingValue() throws Exception {
		XConfigNode xConfigNode = createEmptyXConfig();

		xConfigNode.getRequiredLong("value");
	}

	@Test
	public void testRequiredStringFromValidValue() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("\"any string\""));

		String result = xConfigNode.getRequiredString("subnode", "value");

		assertEquals("any string", result);
	}

	@Test(expected=InvalidConfigException.class)
	public void testRequiredStringFromMissingValue() throws Exception {
		XConfigNode xConfigNode = createEmptyXConfig();

		xConfigNode.getRequiredString("value");
	}

	@Test
	public void testRequiredBooleanFromValidValue() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("true"));

		boolean result = xConfigNode.getRequiredBoolean("subnode", "value");

		assertEquals(true, result);
	}

	@Test(expected=InvalidConfigException.class)
	public void testRequiredInvalidBoolean() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("123"));

		xConfigNode.getRequiredBoolean("subnode", "value");
	}

	@Test(expected=InvalidConfigException.class)
	public void testRequiredBooleanFromMissingValue() throws Exception {
		XConfigNode xConfigNode = createEmptyXConfig();

		xConfigNode.getRequiredBoolean("value");
	}

	@Test
	public void testRequiredBigDecimalFromNumericValue() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("123.45"));

		BigDecimal result = xConfigNode.getRequiredBigDecimal("subnode", "value");

		assertEquals(new BigDecimal("123.45"), result);
	}

	@Test(expected=InvalidConfigException.class)
	public void testRequiredInvalidBigDecimal() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("invalid_number"));

		xConfigNode.getRequiredBigDecimal("subnode", "value");
	}

	@Test(expected=InvalidConfigException.class)
	public void testRequiredBigDecimalFromMissingValue() throws Exception {
		XConfigNode xConfigNode = createEmptyXConfig();

		xConfigNode.getRequiredBigDecimal("value");
	}
	
	@Test
	public void testSubNodeStreamFromAListContainingValidValues() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("[{\"integer\":123}, {\"string\": \"hello\"}]"));

		Stream<XConfigNode> result = xConfigNode.getSubNodeStream("subnode", "value");

		XConfigNode[] resultArray = result.toArray(size -> new XConfigNode[size]);
		assertEquals(2, resultArray.length);
		assertEquals(123, resultArray[0].getRequiredInteger("integer"));
		assertEquals(Optional.of("hello"), resultArray[1].getString("string"));
	}

	@Test
	public void testInvalidSubNodeStream() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("invalid_list"));

		Stream<XConfigNode> result = xConfigNode.getSubNodeStream("subnode", "value");

		assertEquals(0, result.count());
	}
	
	@Test
	public void testMissingSubNodeStream() throws Exception {
		XConfigNode xConfigNode = createEmptyXConfig();

		Stream<XConfigNode> result = xConfigNode.getSubNodeStream("value");

		assertEquals(0, result.count());
	}

	@Test
	public void testGetBreededNodeForBreededValue() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValueAndBreed("1234", "5678"));

		XConfigNode breededNode = xConfigNode.getBreededNode(BREEDS_ARRAY);
		String result = breededNode.getString("subnode", "value").get();

		assertEquals("5678", result);
	}
	
	@Test
	public void testGetBreededNodeForNotBreededValue() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("1234"));

		XConfigNode breededNode = xConfigNode.getBreededNode(BREEDS_ARRAY);
		String result = breededNode.getString("subnode", "value").get();

		assertEquals("1234", result);
	}
	
	@Test
	public void testGetBreededNodeForDifferentlyBreededValue() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("1234"));

		XConfigNode breededNode = xConfigNode.getBreededNode(OTHER_BREEDS_ARRAY);
		String result = breededNode.getString("subnode", "value").get();

		assertEquals("1234", result);
	}
	
	@Test
	public void testGetBreededNodeWithMapForBreededValue() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValueAndBreed("1234", "5678"));

		XConfigNode breededNode = xConfigNode.getBreededNode(BREEDS_MAP);
		String result = breededNode.getString("subnode", "value").get();

		assertEquals("5678", result);
	}
	
	@Test(expected=UnsupportedOperationException.class)
	public void testGetBreededNodeWithMapForStreamChildNode() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("[{\"integer\":123}, {\"string\": \"hello\"}]"));
		
		XConfigNode breededNode = xConfigNode.getBreededNode(BREEDS_MAP);
		breededNode.getSubNodeStream("subnode", "value")
				.findFirst().get()
				.getBreededNode(BREEDS_MAP);
	}
	
	@Test
	public void testGetBreededNodeWithMapForNotBreededValue() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("1234"));

		XConfigNode breededNode = xConfigNode.getBreededNode(BREEDS_MAP);
		String result = breededNode.getString("subnode", "value").get();

		assertEquals("1234", result);
	}
	
	@Test
	public void testGetBreededNodeWithMapForDifferentlyBreededValue() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("1234"));

		XConfigNode breededNode = xConfigNode.getBreededNode(OTHER_BREEDS_MAP);
		String result = breededNode.getString("subnode", "value").get();

		assertEquals("1234", result);
	}

	@Test
	public void testGetFromCurrentNode() {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("true"));

		XConfigNode rootNode = xConfigNode.getSubNode();
		XConfigNode valueNode = rootNode.getSubNode("subnode", "value");
		boolean result = valueNode.getBoolean().get();

		assertTrue(result);
	}
	
	private String yamlWithValue(String value) {
		return "rootNode:\n" +
				"  subnode:\n" +
				"    value: " + value;
	}

	private String yamlWithValueAndBreed(String defaultValue, String breededValue) {
		return "rootNode:\n" +
				"  subnode:\n" +
				"    value: " + defaultValue + "\n" +
				"rootNode_breeds:\n" +
				"  breedKey:\n" +
				"    breedValue:\n" +
				"      subnode:\n" +
				"        value: " + breededValue;
	}

	private XConfigNode createXConfigNode(String yamlString) {
		XConfig xconfig = new YamlXConfig(yamlString);
		XConfigNode.Factory factory = new XConfigNode.Factory(() -> xconfig);
		return factory.get(ROOT_NODE);
	}


	private XConfigNode createEmptyXConfig() {
		XConfig xconfig = new YamlXConfig("rootNode: ~");
		XConfigNode.Factory factory = new XConfigNode.Factory(() -> xconfig);
		return factory.get(ROOT_NODE);
	}
}
