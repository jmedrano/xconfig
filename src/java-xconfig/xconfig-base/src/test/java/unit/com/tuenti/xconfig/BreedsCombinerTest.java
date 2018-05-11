package com.tuenti.xconfig;

import static org.junit.Assert.*;

import org.junit.Test;

public class BreedsCombinerTest {
	
	private BreedsCombiner merger = new BreedsCombiner();

	@Test
	public void testCombineBreedsWithOneParentBreed() {
		String[][] result = merger.combineBreeds(
				breeds("virtualEnv", "myuser"),
				breeds("foo", "fooValue",
						"fii", "fiiValue"));
		
		assertArrayEquals(breeds(
				"virtualEnv", "myuser",
				"foo", "fooValue",
				"fooVirtualEnv", "fooValue_myuser",
				"fii", "fiiValue",
				"fiiVirtualEnv", "fiiValue_myuser"),
			result);
	}
	
	@Test
	public void testCombineBreedsWithMoreThanOneParentBreed() {
		String[][] result = merger.combineBreeds(
				breeds("virtualEnv", "myuser",
						"baseWhatever", "baseWhateverValue"),
				breeds("foo", "fooValue",
						"fii", "fiiValue"));
		
		assertArrayEquals(breeds(
				"virtualEnv", "myuser",
				"baseWhatever", "baseWhateverValue",
				"foo", "fooValue",
				"fooVirtualEnv", "fooValue_myuser",
				"fooBaseWhatever", "fooValue_baseWhateverValue",
				"fii", "fiiValue",
				"fiiVirtualEnv", "fiiValue_myuser",
				"fiiBaseWhatever", "fiiValue_baseWhateverValue"),
			result);
	}
	
	private String[][] breeds(String... keyValue) {
		String[][] result = new String[keyValue.length / 2][2];
		for (int i=0; i<keyValue.length / 2; i++) {
			result[i][0] = keyValue[i*2];
			result[i][1] = keyValue[i*2 +1];
		}
		return result;
	}
}
