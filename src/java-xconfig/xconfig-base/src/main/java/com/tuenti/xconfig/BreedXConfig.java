package com.tuenti.xconfig;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;
import com.tuenti.xconfig.type.XConfigMap;
import com.tuenti.xconfig.type.XConfigValue;

/**
 * BreedXConfig class. Acts as a wrapper for an XConfig instance,
 * converting each individual petition into all the petitions required
 * by a specified breed.
 */
public class BreedXConfig extends XConfigBase {

	private XConfig xconfig;
	private String[][] breed;

	public BreedXConfig(XConfig xconfig, String[][] breed) {
		if (xconfig instanceof BreedXConfig) {	
			throw new RuntimeException("You can't pass a BreedsXConfig to another BreedsXConfig");
		}
		this.xconfig = xconfig;
		this.breed = breed;
	}

	@Override
	public void close() {
		xconfig.close();
	}

	@Override
	public boolean reload() {
		return xconfig.reload();
	}

	@Override
	public XConfigValue getValue(String key) throws XConfigKeyNotFoundException {
		XConfigValue value = null;
		boolean isMap = true;
		boolean found = false;

		String[] breedKeys = getBreedKeys(key);
		
		// Iterate the keys in reverse order
		// We only continue iterating if we received a map
		for (int i=breedKeys.length-1; i>=0 && isMap; i--) {
			String breedKey = breedKeys[i];
			
			try {
				XConfigValue newValue = xconfig.getValue(breedKey);
				isMap = newValue instanceof XConfigMap;
				value = found ? calculateOverride(newValue, value) : newValue;
				found = true;
			} catch (XConfigKeyNotFoundException e) { }
		}

		if (!found) {
			throw new XConfigKeyNotFoundException(key);
		}

		return value;
	}

	@Override
	public boolean hasKey(String key) {
		String[] breedKeys = getBreedKeys(key);

		for (String breedKey : breedKeys) {
			if (xconfig.hasKey(breedKey)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public long getLastModificationTime(String key) throws XConfigKeyNotFoundException {
		// Still not 
		return 0;
	}

	/**
	 * Get all the keys we will be trying when getting a specified breed
	 * @param originalKey
	 * @return
	 */
	private String[] getBreedKeys(String originalKey) {
		int keyCount = breed.length + 1;
		String[] keys = new String[keyCount];

		String[] originalPath = originalKey.split("/", 2);
		String oldHead = originalPath[0];
		String newHead = oldHead + "_breeds";

		keys[0] = originalKey;
		for (int i=0; i<breed.length; i++) {
			String[] pair = breed[i];
			String newKey = newHead + "/" + pair[0] + "/" + pair[1];
			if (originalPath.length >= 2) {
				newKey += "/" + originalPath[1];
			}
			keys[i+1] = newKey;
		}
		return keys;
	}

	private XConfigValue calculateOverride(XConfigValue baseValue, XConfigValue overrideValue) {
		if (baseValue != null && overrideValue != null) {
			try {
				XConfigMap baseMap = baseValue.getAsMap();
				XConfigMap overrideMap = overrideValue.getAsMap();

				if (baseMap != null && overrideMap != null) {
					baseMap.overrideWith(overrideMap);
					return baseMap;
				}
			} catch (XConfigWrongTypeCastingException e) { }
		}
		return overrideValue;
	}
}
