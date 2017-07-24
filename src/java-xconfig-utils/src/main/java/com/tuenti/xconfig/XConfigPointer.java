package com.tuenti.xconfig;

import static com.tuenti.xconfig.XConfigPath.XCConcat;

import java.util.Optional;

import javax.inject.Provider;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigValue;

interface XConfigPointer {
	
	Optional<XConfigValue> getValue(String... path);
	

	static class RootPointer implements XConfigPointer {
		private final Provider<XConfig> xConfigProvider;
		private final String[] rootPath;
		
		public RootPointer(Provider<XConfig> xConfigProvider, String[] rootPath) {
			this.xConfigProvider = xConfigProvider;
			this.rootPath = rootPath;
		}
		
		public Optional<XConfigValue> getValue(String... path) {
			try {
				return Optional.of(xConfigProvider.get().getValue(concat(path)));
			} catch (XConfigKeyNotFoundException e) {
				return Optional.empty();
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
