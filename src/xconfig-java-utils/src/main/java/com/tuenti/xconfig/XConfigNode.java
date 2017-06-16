package com.tuenti.xconfig;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import javax.inject.Inject;
import javax.inject.Provider;

import static com.tuenti.xconfig.XConfigPath.XCConcat;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigList;
import com.tuenti.xconfig.type.XConfigMap;
import com.tuenti.xconfig.type.XConfigValue;

public final class XConfigNode {
	private final Provider<XConfig> xConfigProvider;
	private final String[] rootPath;

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

	public XConfigNode(Provider<XConfig> xConfigProvider, String... rootPath) {
		this.xConfigProvider = xConfigProvider;
		this.rootPath = rootPath;
	}

	public Optional<String> getString(String... path) {
		return Optional.ofNullable(xConfigProvider.get().getAsString(concat(path), null));
	}

	public Optional<Integer> getInteger(String... path) {
		return Optional.ofNullable(xConfigProvider.get().getAsInteger(concat(path), null));
	}

	public Optional<Boolean> getBoolean(String... path) {
		return Optional.ofNullable(xConfigProvider.get().getAsBoolean(concat(path), null));
	}

	public Optional<BigDecimal> getBigDecimal(String... path) {
		return Optional.ofNullable(xConfigProvider.get().getAsString(concat(path), null))
				.map(this::toBigDecimal);
	}

	private BigDecimal toBigDecimal(String input) {
		try {
			return new BigDecimal(input);
		} catch (NumberFormatException e) {
			return null;
		}
	}

	/**
	 * Get a new XConfigNode(newRootPath), being newRootPath = previousRootPath + path
	 */
	public XConfigNode getSubNode(String... path) {
		return new XConfigNode(xConfigProvider, concat(path));
	}

	public Stream<Integer> getIntegerStream(String... path) {
		Function<XConfigValue, Integer> mapper = (i) -> {
			try {
				return i.getAsInteger();
			} catch (XConfigWrongTypeCastingException e) {
				return null;
			}
		};
		return getStream(mapper, path).filter(Objects::nonNull);
	}

	public Stream<BigDecimal> getBigDecimalStream(String... path) {
		Function<XConfigValue, BigDecimal> mapper = (i) -> {
			try {
				return toBigDecimal(i.getAsString());
			} catch (XConfigWrongTypeCastingException e) {
				return null;
			}
		};

		return getStream(mapper, path).filter(Objects::nonNull);
	}

	public Stream<String> getStringStream(String... path) {
		Function<XConfigValue, String> mapper = (i) -> {
			try {
				return i.getAsString();
			} catch (XConfigWrongTypeCastingException e) {
				return null;
			}
		};
		return getStream(mapper, path).filter(Objects::nonNull);
	}

	public Map<String, XConfigNode> getMapOfNodes(String... path) {
		XConfigMap map = null;
		try {
			map = xConfigProvider.get().getAsMap(concat(path));
		} catch (XConfigKeyNotFoundException | XConfigWrongTypeCastingException e) {
			return new HashMap<>();
		}

		Function<Entry<String, Object>, XConfigNode> toXConfigNode = i ->
				new XConfigNode(xConfigProvider, concat(XCConcat(path), i.getKey()));

		return map.getAsJavaObject()
				.entrySet()
				.stream()
				.collect(Collectors.toMap(i -> i.getKey(), toXConfigNode));
	}

	private <T> Stream<T> getStream(Function<XConfigValue, T> mapper, String... path) {
		XConfigList xconfigList = xConfigProvider.get().getAsList(concat(path), null);
		if (xconfigList == null || xconfigList.isEmpty()) {
			return Stream.empty();
		}

		Stream<XConfigValue> xConfigValueStream = StreamSupport.stream(xconfigList.spliterator(), false);
		return xConfigValueStream.map(mapper);
	}

	private String concat(String... parts) {
		return XCConcat(XCConcat(rootPath), XCConcat(parts));
	}
}
