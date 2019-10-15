/**
 * XConfigPath.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig;

import static com.tuenti.xconfig.utils.StringUtils.replace;

/**
 * XConfigPath class. This class has a unique public method in order to generate
 * a valid XConfig path giving the key per parts.
 */
public class XConfigPath {
	private static final String SEPARATOR = "/";
	private String path;

	/**
	 * Create a path object concatenating pieces. Pieces will be escaped.
	 *
	 * @param pieces
	 */
	public XConfigPath(Object... pieces) {
		path = XCJoin(pieces);
	}

	/**
	 * Return the path as a string extending it the given pieces (all escaped).
	 *
	 * @param pieces
	 * @return
	 */
	public String extend(Object... pieces) {
		return path + SEPARATOR + XCJoin(pieces);
	}

	@Override
	public String toString() {
		return path;
	}

	/**
	 * Create a key concatenating the given parts. Does not escape components
	 * use {@link #XCJoin} instead.
	 *
	 * @param pieces
	 * @return Valid key path
	 */
	public static final String XCConcat(Object... pieces) {
		StringBuilder path = new StringBuilder();
		for (int i = 0; i < pieces.length; i++) {
			if (i > 0) {
				path.append(SEPARATOR);
			}
			path.append(pieces[i]);
		}
		return path.toString();
	}

	/**
	 * Retrieve a valid key given by parts. Parts containing / will be escaped
	 *
	 * @param pieces
	 * @return Valid key path
	 */
	public static final String XCJoin(Object... pieces) {
		for (int j = 0; j < pieces.length; j++) {
			pieces[j] = XConfigPath.escape(pieces[j].toString());
		}
		return XCConcat(pieces);
	}

	/**
	 * This method escapes the characters that need to be escaped.
	 *
	 * @param string
	 * @return Escaped string
	 */
	private static final String escape(String string) {
		return replace(replace(replace(string, "\\", "\\\\"), "/", "\\/"), "#", "\\#");
	}
}
