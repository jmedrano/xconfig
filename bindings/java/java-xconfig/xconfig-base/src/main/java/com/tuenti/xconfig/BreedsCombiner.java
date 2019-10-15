package com.tuenti.xconfig;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

/**
 * Class able to combine two levels of breeds (parent / child), adding them both individually and
 * in combinations of parent/child keys, with the form:
 * <li> xxx_breeds/childKey/childValue
 * <li> xxx_breeds/parentKey/parentValue 
 * <li> xxx_breeds/childKeyParentKey/childValue_parentValue
 */
public class BreedsCombiner {

	public String[][] combineBreeds(String[][] parentBreeds, String[][] childBreeds) {
		List<String[]> result = new ArrayList<>();
		for (String[] baseEntry : parentBreeds) {
			addBreed(result, baseEntry[0], baseEntry[1]);
		}
		for (String[] override : childBreeds) {
			addBreed(result, override[0], override[1]);
		
			for (String[] baseEntry : parentBreeds) {
				String combinedKey = getCombinedKey(baseEntry[0], override[0]);
				addBreed(result, combinedKey, override[1] + "_" + baseEntry[1]);
			}
		}
		return result.toArray(new String[0][0]);
	}
	
	private void addBreed(List<String[]> breeds, String breedKey, String breedValue) {
		breeds.add(new String[] {breedKey, breedValue});
	}
	
	private String getCombinedKey(String baseKey, String override) {
		String capitalizedBaseKey = Character.toUpperCase(baseKey.charAt(0)) + baseKey.substring(1);
		return override + capitalizedBaseKey;
	}
}
