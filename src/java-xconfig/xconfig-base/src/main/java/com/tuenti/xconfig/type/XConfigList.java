/*
 * XConfigList.java
 *
 * Copyright (C) 2014 Tuenti Technologies S.L.
 *
 * This file can only be stored on servers belonging to Tuenti Technologies S.L.
 */
package com.tuenti.xconfig.type;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;


/**
 * XConfigList class.
 */
public class XConfigList implements XConfigValue, Iterable<XConfigValue> {

	private List<XConfigValue> values;

	public XConfigList() {
		this.values = new ArrayList<>();
	}

	public XConfigList(List<XConfigValue> list) {
		this.values = new ArrayList<>();
		this.values.addAll(list);
	}

	/*
	 * XConfigValue methods
	 */

	@Override
	public XConfigList getAsList() {
		return this;
	}

	@Override
	public XConfigMap getAsMap() throws XConfigWrongTypeCastingException {
		throw new XConfigWrongTypeCastingException();
	}

	@Override
	public Integer getAsInteger() throws XConfigWrongTypeCastingException {
		throw new XConfigWrongTypeCastingException();
	}

	@Override
	public String getAsString() throws XConfigWrongTypeCastingException {
		throw new XConfigWrongTypeCastingException();
	}

	@Override
	public Float getAsFloat() throws XConfigWrongTypeCastingException {
		throw new XConfigWrongTypeCastingException();
	}

	@Override
	public Boolean getAsBoolean() throws XConfigWrongTypeCastingException {
		throw new XConfigWrongTypeCastingException();
	}

	@Override
	public XConfigValueType getType() {
		return XConfigValueType.LIST;
	}

	@Override
	public String toString() {
		return "XConfigList [values=" + values + "]";
	}

	/*
	 * Own methods
	 */

	public void add(XConfigValue value) {
		this.values.add(value);
	}

	/*
	 * Delegated methods to List
	 */

	public boolean containsAll(Collection<?> c) {
		return values.containsAll(c);
	}

	public XConfigValue get(int index) throws XConfigKeyNotFoundException {
		if (index < 0 || index >= values.size()) {
			throw new XConfigKeyNotFoundException(Integer.toString(index));
		}
		return values.get(index);
	}

	public int indexOf(Object o) {
		return values.indexOf(o);
	}

	public int lastIndexOf(Object o) {
		return values.lastIndexOf(o);
	}

	public ListIterator<XConfigValue> listIterator() {
		return values.listIterator();
	}

	public ListIterator<XConfigValue> listIterator(int index) {
		return values.listIterator(index);
	}

	public List<XConfigValue> subList(int fromIndex, int toIndex) {
		return values.subList(fromIndex, toIndex);
	}

	public int size() {
		return values.size();
	}

	public boolean isEmpty() {
		return values.isEmpty();
	}

	public boolean contains(Object o) {
		return values.contains(o);
	}

	public Object[] toArray() {
		return values.toArray();
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;

		XConfigList that = (XConfigList) o;

		if (!values.equals(that.values)) return false;

		return true;
	}

	@Override
	public int hashCode() {
		return values.hashCode();
	}

	/*
	 * Iterator interface methods
	 */

	@Override
	public Iterator<XConfigValue> iterator() {
		return values.iterator();
	}

	@Override
	public List<Object> getAsJavaObject() {
		List<Object> javaValues = new LinkedList<>();
		for (XConfigValue v : values) {
			javaValues.add(v.getAsJavaObject());
		}
		return javaValues;
	}
}
