package com.tuenti.xconfig;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import javax.inject.Inject;
import javax.inject.Provider;

import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigList;
import com.tuenti.xconfig.type.XConfigMap;
import com.tuenti.xconfig.type.XConfigValue;

public final class XConfigNode {
	private final XConfigPointer pointer;

	public static class Factory {
		private final Provider<XConfig> provider;

		@Inject
		public Factory(Provider<XConfig> provider) {
			this.provider = provider;
		}

		public XConfigNode get(String... rootPath) {
			return new XConfigNode(provider, rootPath);
		}
	}

	private XConfigNode(XConfigPointer pointer) {
		this.pointer = pointer;
	}

	public XConfigNode(Provider<XConfig> xConfigProvider, String... rootPath) {
		this.pointer = new XConfigPointer.RootPointer(xConfigProvider, rootPath);
	}

	/**
	 * Returns the same node but with the content customized by the given breeds, received as an array.
	 * Should only be used with root nodes, as no correct behavior is guaranteed otherwise.
	 */
	public XConfigNode getBreededNode(String[][] breeds) {
		return new XConfigNode(pointer.getBreededPointer(breeds));
	}

	/**
	 * Returns the same node but with the content customized by the given breeds, received as a Map.
	 * Should only be used with root nodes, as no correct behavior is guaranteed otherwise.
	 */
	public XConfigNode getBreededNode(Map<String, String> breeds) {
		return getBreededNode(breeds.entrySet().stream()
				.map(e -> new String[] {e.getKey(), e.getValue()})
				.toArray(size -> new String[size][2]));
	}

	public Optional<String> getString(String... path) {
		return pointer.getValue(path).map(this::toString);
	}

	public Optional<Integer> getInteger(String... path) {
		return pointer.getValue(path).map(this::toInteger);
	}

	public Optional<Long> getLong(String... path) {
		return pointer.getValue(path).map(this::toLong);
	}

	public Optional<Boolean> getBoolean(String... path) {
		return pointer.getValue(path).map(this::toBoolean);
	}

	public Optional<BigDecimal> getBigDecimal(String... path) {
		return pointer.getValue(path).map(this::toBigDecimal);
	}

	public String getRequiredString(String... path) {
		return require(getString(path), path);
	}

	public int getRequiredInteger(String... path) {
		return require(getInteger(path), path);
	}

	public long getRequiredLong(String... path) {
		return require(getLong(path), path);
	}

	public boolean getRequiredBoolean(String... path) {
		return require(getBoolean(path), path);
	}

	public BigDecimal getRequiredBigDecimal(String... path) {
		return require(getBigDecimal(path), path);
	}

	/**
	 * Get a new XConfigNode(newRootPath), being newRootPath = previousRootPath + path
	 */
	public XConfigNode getSubNode(String... path) {
		return new XConfigNode(new XConfigPointer.ChildPointer(pointer.getValue(path)));
	}

	/**
	 * Get a stream of XConfigNode objects with their root in each child element of a XConfig list path.
	 */
	public Stream<XConfigNode> getSubNodeStream(String... path) {
		return getStream(this::toNode, path).filter(Objects::nonNull);
	}

	public Stream<Integer> getIntegerStream(String... path) {
		return getStream(this::toInteger, path).filter(Objects::nonNull);
	}

	public Stream<Long> getLongStream(String... path) {
		return getStream(this::toLong, path).filter(Objects::nonNull);
	}

	public Stream<BigDecimal> getBigDecimalStream(String... path) {
		return getStream(this::toBigDecimal, path).filter(Objects::nonNull);
	}

	public Stream<String> getStringStream(String... path) {
		return getStream(this::toString, path).filter(Objects::nonNull);
	}

	public Map<String, XConfigNode> getMapOfNodes(String... path) {
		try {
			return pointer.getValue(path).orElse(new XConfigMap())
					.getAsMap().entrySet()
					.stream()
					.collect(Collectors.toMap(
							entry -> entry.getKey(), entry -> toNode(entry.getValue())));
			
		} catch (XConfigWrongTypeCastingException e) {
			return Collections.emptyMap();
		}
	}

	private <T> T require(Optional<T> value, String... path) {
		return value.orElseThrow(() -> new InvalidConfigException("Required config value not found or invalid in "
					+ String.join("/", path)));
	}

	private String toString(XConfigValue input) {
		return valueOrNullIfError(input, (it -> it.getAsString()));
	}

	private BigDecimal toBigDecimal(XConfigValue input) {
		return valueOrNullIfError(input, (it -> new BigDecimal(it.getAsString())));
	}

	private Long toLong(XConfigValue input) {
		return valueOrNullIfError(input, (it -> Long.valueOf(it.getAsString())));
	}

	private Integer toInteger(XConfigValue input) {
		return valueOrNullIfError(input, (it -> Integer.valueOf(it.getAsString())));
	}

	private Boolean toBoolean(XConfigValue input) {
		return valueOrNullIfError(input, (it -> it.getAsBoolean()));
	}
	
	private <T> T valueOrNullIfError(XConfigValue input, Function<XConfigValue, T> valueFunction) {
		try {
			return valueFunction.apply(input);
		} catch (NumberFormatException | XConfigWrongTypeCastingException e) {
			return null;
		}
	}

	private XConfigNode toNode(XConfigValue value) {
		return new XConfigNode(new XConfigPointer.ChildPointer(Optional.of(value)));
	}

	private <T> Stream<T> getStream(Function<XConfigValue, T> mapper, String... path) {
		try {
			XConfigList xconfigList = pointer.getValue(path)
					.map(value -> value.getAsList())
					.orElse(new XConfigList());
	
			return StreamSupport.stream(xconfigList.spliterator(), false).map(mapper);
			
		} catch (XConfigWrongTypeCastingException e) {
			return Stream.empty();
		}
	}
}
