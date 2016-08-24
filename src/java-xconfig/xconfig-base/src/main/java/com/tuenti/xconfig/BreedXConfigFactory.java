package com.tuenti.xconfig;

public class BreedXConfigFactory {
	/**
	 * Returns an XConfig wrapper that calculates the values using
	 * the specified breed
	 * @param xconfig The current config
	 * @param breed key-value pairs, priority order is ascending
	 * @return The XConfig wrapper
	 */
	public static BreedXConfig get(XConfig xconfig, final String[][] breed) {
		return new BreedXConfig(xconfig, breed);
	}
}
