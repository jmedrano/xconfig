package com.tuenti.xconfig.exception;

public class XConfigKeyNotFoundException extends Exception {
	private static final long serialVersionUID = 1L;
    private String key;
    public XConfigKeyNotFoundException(String key) {
        super(String.format("Not found key: \"%s\"", key));
        this.key = key;
    }
    public String getNotFoundKey() {
        return this.key;
    }
}