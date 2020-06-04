package com.tuenti.xconfig.exception;

public class XConfigKeyNotFoundException extends RuntimeException {
	private static final long serialVersionUID = 1L;

	private final String key;

	/* We are not filling the stack trace for this exception as it can be cached and could contain the wrong stack trace */
	public XConfigKeyNotFoundException(String key) {
		super("Not found key: \"" + key + "\"", null, true, false);
		this.key = key;
	}

	public String getNotFoundKey() {
		return this.key;
	}

	@Override
	public synchronized Throwable fillInStackTrace() {
		return this;
	}
}
