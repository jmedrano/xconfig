package com.tuenti.xconfig;

import static com.tuenti.xconfig.XConfigPath.XCConcat;

import java.util.Optional;

import javax.inject.Provider;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigValue;

interface XConfigPointer {
	
	Optional<XConfigValue> getValue(String... path);
	
	XConfigPointer getBreededPointer(String[][] breeds);


	static class RootPointer implements XConfigPointer {
		private final Provider<XConfig> xConfigProvider;
		private final String[] rootPath;
		private final String[][] breeds;
		
		public RootPointer(Provider<XConfig> xConfigProvider, String[] rootPath) {
			this.xConfigProvider = xConfigProvider;
			this.rootPath = rootPath;
			this.breeds = null;
		}
		
		private RootPointer(Provider<XConfig> xConfigProvider, String[] rootPath, String[][] breeds) {
			this.xConfigProvider = xConfigProvider;
			this.rootPath = rootPath;
			this.breeds = breeds;
		}
		
		public XConfigPointer getBreededPointer(String[][] breeds) {
			return new RootPointer(xConfigProvider, rootPath, breeds);
		}
		
		public Optional<XConfigValue> getValue(String... path) {
			try {
				return Optional.of(getXConfig().getValue(concat(path)));
			} catch (XConfigKeyNotFoundException e) {
				return Optional.empty();
			}
		}
		
		private XConfig getXConfig() {
			if (breeds == null) {
				return xConfigProvider.get();
			} else {
				return new BreedXConfig(xConfigProvider.get(), breeds);
			}
		}
		
		private String concat(String... parts) {
			return XCConcat(XCConcat((Object[]) rootPath), XCConcat((Object[]) parts));
		}
	}

	static class ChildPointer implements XConfigPointer {
		private final Optional<XConfigValue> rootValue;
		
		public ChildPointer(Optional<XConfigValue> rootValue) {
			this.rootValue = rootValue;
		}

		public XConfigPointer getBreededPointer(String[][] breeds) {
			throw new UnsupportedOperationException("Breeded config nodes can only be generated from root nodes");
		}
		
		public Optional<XConfigValue> getValue(String... path) {
			return rootValue.flatMap((value) -> getValue(value, path));
		}
		
		private Optional<XConfigValue> getValue(XConfigValue value, String... path) {
			try {
				XConfigValue current = value;
				for (String pathPiece : path) {
					current = current.getAsMap().get(pathPiece);
				}
				return Optional.of(current);
				
			} catch (XConfigKeyNotFoundException | XConfigWrongTypeCastingException e) {
				return Optional.empty();
			}
		}
	}
}
