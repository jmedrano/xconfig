package com.tuenti.xconfig.exception;

public class XConfigKeyNotFoundException extends Exception {
	private static final long serialVersionUID = 1L;

    private String key;

    public XConfigKeyNotFoundException(String key) {
        super("Not found key: \"" + key + "\"");
        this.key = key;
    }

    public String getNotFoundKey() {
        return this.key;
    }
}
