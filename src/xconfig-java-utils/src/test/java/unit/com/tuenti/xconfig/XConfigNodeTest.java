package com.tuenti.xconfig;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import org.junit.Test;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import com.tuenti.xconfig.parser.YamlXConfig;

public class XConfigNodeTest {

	private static final String ROOT_NODE = "rootNode";

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
	public void testString() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("\"any string\""));

		String result = xConfigNode.getString("subnode", "value").get();

		assertEquals("any string", result);
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
	public void testSubnode() throws Exception {
		XConfigNode xConfigNode = createXConfigNode(yamlWithValue("{\"integer\":123}"));

		XConfigNode result = xConfigNode.getSubNode("subnode", "value");

		assertEquals(Optional.of(123), result.getInteger("integer"));
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

		assertEquals(new HashMap(), result);
	}

	private String yamlWithValue(String value) {
		return "rootNode:\n" +
				"  subnode:\n" +
				"    value: " + value;
	}

	private XConfigNode createXConfigNode(String yamlString) {
		XConfig xconfig = new YamlXConfig(yamlString);
		XConfigNode.Factory factory = new XConfigNode.Factory(() -> xconfig);
		return factory.get(ROOT_NODE);
	}
}