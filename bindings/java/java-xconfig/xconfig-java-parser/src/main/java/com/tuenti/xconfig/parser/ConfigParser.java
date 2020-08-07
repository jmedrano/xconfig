package com.tuenti.xconfig.parser;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.yaml.snakeyaml.Yaml;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigBoolean;
import com.tuenti.xconfig.type.XConfigFloat;
import com.tuenti.xconfig.type.XConfigInteger;
import com.tuenti.xconfig.type.XConfigList;
import com.tuenti.xconfig.type.XConfigLong;
import com.tuenti.xconfig.type.XConfigMap;
import com.tuenti.xconfig.type.XConfigNull;
import com.tuenti.xconfig.type.XConfigString;
import com.tuenti.xconfig.type.XConfigValue;

public class ConfigParser {
	private final Yaml yaml = new Yaml();

	private long configHash = 0;
	private XConfigMap config = new XConfigMap();

	public long getConfigHash() {
		return configHash;
	}

	public void addFile(File file) throws FileNotFoundException {
		FileInputStream fis = new FileInputStream(file);
		Object load = yaml.load(fis);
		try {
			XConfigValue xMap = convertToXConfig(load);
			XConfigMap otherMap = xMap.getAsMap();
			config = config.mergeWith(otherMap);
			// in XConfig native this behaves like a hash and not a timestamp
			configHash = (configHash * 31) + file.lastModified();
		} catch (XConfigWrongTypeCastingException e) {
			throw new RuntimeException("File " + file + " has an incorrect fomat");
		} finally {
			try {
				fis.close();
			} catch (IOException ignored) {
			}
		}
	}
	
	public void addYamlString(String yamlString) {
		Object load = yaml.load(yamlString);
		addYamlFromJavaObject(load);
	}

	public void addYamlFromJavaObject(Object javaObject) {
		try {
			XConfigValue xMap = convertToXConfig(javaObject);
			XConfigMap otherMap = xMap.getAsMap();
			config = config.mergeWith(otherMap);
			
		} catch (XConfigWrongTypeCastingException e) {
			throw new RuntimeException("Yaml has an incorrect fomat");
		}
	}

	public XConfigValue getElement(String path) throws XConfigKeyNotFoundException {
		try {
			return getElement(config, path);
		} catch (XConfigKeyNotFoundException | XConfigWrongTypeCastingException e) {
			// re-throw with full path
			throw new XConfigKeyNotFoundException(path);
		}
	}

	private XConfigValue getElement(XConfigMap conf, String path)
			throws XConfigKeyNotFoundException, XConfigWrongTypeCastingException {
		XConfigMap confAsMap = conf.getAsMap();

		if (path.contains("/")) {
			String[] split = path.split("/", 2);
			String head = split[0];
			String rest = split[1];

			if (!confAsMap.containsKey(head)) {
				throw new XConfigKeyNotFoundException(null);
			}
			XConfigValue subMap = confAsMap.get(head);
			if (subMap instanceof XConfigMap) {
				XConfigMap sMap = (XConfigMap) subMap;
				return getElement(sMap, rest);
			} else {
				throw new XConfigKeyNotFoundException(null);
			}
		} else {
			if (!confAsMap.containsKey(path)) {
				throw new XConfigKeyNotFoundException(path);
			}
			return confAsMap.get(path);
		}
	}

	private XConfigValue convertToXConfig(Object element) {
		if (element == null) {
			return new XConfigNull();
		}

		if (element instanceof Integer) {
			return new XConfigInteger((Integer) element);
		}

		if (element instanceof Long) {
			return new XConfigLong((Long) element);
		}

		if (element instanceof Boolean) {
			return new XConfigBoolean((Boolean) element);
		}

		if (element instanceof Float) {
			return new XConfigFloat((Float) element);
		}

		if (element instanceof Double) {
			return new XConfigFloat(((Double) element).floatValue());
		}

		if (element instanceof String) {
			return new XConfigString((String) element);
		}

		if (element instanceof List<?>) {
			List<?> list = (List<?>) element;
			List<XConfigValue> backingList = new ArrayList<>(list.size());
			for (Object object : list) {
				backingList.add(convertToXConfig(object));
			}
			return XConfigList.wrapping(backingList);
		}

		if (element instanceof Map<?, ?>) {
			Map<?, ?> map = (Map<?, ?>) element;
			Map<String, XConfigValue> backingMap = new HashMap<>(map.size());
			for (Entry<?, ?> entry : map.entrySet()) {
				String key = String.valueOf(entry.getKey());
				XConfigValue value = convertToXConfig(entry.getValue());
				backingMap.put(key, value);
			}
			return XConfigMap.wrapping(backingMap);
		}

		throw new RuntimeException("Unexpected value " + element + " of type " + element.getClass());
	}
}
