package com.tuenti.xconfig;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.yaml.snakeyaml.Yaml;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigBoolean;
import com.tuenti.xconfig.type.XConfigFloat;
import com.tuenti.xconfig.type.XConfigInteger;
import com.tuenti.xconfig.type.XConfigList;
import com.tuenti.xconfig.type.XConfigMap;
import com.tuenti.xconfig.type.XConfigNull;
import com.tuenti.xconfig.type.XConfigString;
import com.tuenti.xconfig.type.XConfigValue;

public class ConfigParser {
	private long lastModified = 0;

	Yaml yaml = new Yaml();
	XConfigMap config = new XConfigMap();

	public long getLastModified() {
		return lastModified;
	}

	public void addFile(File file) throws FileNotFoundException {
		Object load = yaml.load(new FileInputStream(file));
		try {
			XConfigValue xMap = convertToXConfig(load);
			XConfigMap otherMap = xMap.getAsMap();
			merge(config, otherMap);
			lastModified = Math.max(lastModified, file.lastModified());
		} catch (XConfigWrongTypeCastingException e) {
			throw new RuntimeException("File " + file + " has an incorrect fomat");
		}

	}

	private void merge(XConfigMap thisMap, XConfigMap otherMap) {
		Set<Entry<String, XConfigValue>> entrySet = otherMap.entrySet();
		for (Entry<String, XConfigValue> entry : entrySet) {
			XConfigValue value = entry.getValue();

			String key = entry.getKey();
			XConfigMap thisValue = null;
			XConfigMap otherValueMap = null;
			try {
				thisValue = thisMap.get(key).getAsMap();
				otherValueMap = value.getAsMap();
			} catch (XConfigWrongTypeCastingException e) {
			} catch (XConfigKeyNotFoundException e) {
			}
			if (thisValue != null && otherValueMap != null) {
				merge(thisValue, otherValueMap);
			} else {
				thisMap.add(key, value);
			}
		}
	}

	public XConfigValue getElement(String path) throws XConfigKeyNotFoundException {
		try {
			return getElement(config, path);
		} catch (XConfigKeyNotFoundException e) {
			// re-throw with full path
			throw new XConfigKeyNotFoundException(path);
		} catch (XConfigWrongTypeCastingException e) {
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
			XConfigList xConfigList = new XConfigList();
			for (Object object : list) {
				xConfigList.add(convertToXConfig(object));
			}
			return xConfigList;
		}

		if (element instanceof Map<?, ?>) {
			@SuppressWarnings("unchecked")
			Map<Object, Object> map = (Map<Object, Object>) element;
			Set<Entry<Object, Object>> entries = map.entrySet();
			XConfigMap xConfigMap = new XConfigMap();
			for (Entry<Object, Object> entry : entries) {
				String key = entry.getKey().toString();
				Object value = entry.getValue();
				XConfigValue xValue = convertToXConfig(value);
				xConfigMap.add(key, xValue);
			}
			return xConfigMap;
		}
		throw new RuntimeException("Unexpected value " + element + " of type " + element.getClass());
	}

}
