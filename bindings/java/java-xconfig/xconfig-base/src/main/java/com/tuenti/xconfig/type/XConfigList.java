package com.tuenti.xconfig.type;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.stream.Stream;

import com.tuenti.xconfig.exception.XConfigKeyNotFoundException;
import com.tuenti.xconfig.exception.XConfigWrongTypeCastingException;


/**
 * XConfigList class.
 */
public class XConfigList implements XConfigValue, Iterable<XConfigValue> {

	private final List<XConfigValue> values;
	private List<XConfigValue> unmodifiableView;
	private int cachedHash;

	/**
	 * Creates a empty XConfigList
	 */
	public XConfigList() {
		this.values = Collections.emptyList();
	}

	/**
	 * Creates a XConfigList backed by a copy of the provided {@code list}.
	 */
	public XConfigList(List<XConfigValue> list) {
		this.values = new ArrayList<>(list);
	}

	/**
	 * Creates a XConfigList using the same instance of the {@code backingList} as the source of values.
	 * Use {@link #XConfigList(List)} if you want it to use a copy.
	 */
	public static XConfigList wrapping(List<XConfigValue> backingList) {
		return new XConfigList(backingList, null);
	}

	private XConfigList(List<XConfigValue> backingList, @SuppressWarnings("unused") PrivateMarker marker) {
		this.values = backingList;
	}

	private static class PrivateMarker {}

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
	public Long getAsLong() throws XConfigWrongTypeCastingException {
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

	public int indexOf(XConfigValue o) {
		return values.indexOf(o);
	}

	public int lastIndexOf(XConfigValue o) {
		return values.lastIndexOf(o);
	}

	public int size() {
		return values.size();
	}

	public boolean isEmpty() {
		return values.isEmpty();
	}

	public boolean contains(XConfigValue o) {
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

		return values.equals(that.values);
	}

	@Override
	public int hashCode() {
		int hash = cachedHash;
		if (hash == 0) {
			hash = values.hashCode();
			cachedHash = hash;
		}
		return hash;
	}

	@Override
	public List<Object> getAsJavaObject() {
		List<Object> javaValues = new LinkedList<>();
		for (XConfigValue v : values) {
			javaValues.add(v.getAsJavaObject());
		}
		return javaValues;
	}

	public Stream<XConfigValue> stream() {
		return values.stream();
	}

	/*
	 * Following methods return a unmodifiable view
	 */

	@Override
	public Iterator<XConfigValue> iterator() {
		return unmodifiableView().iterator();
	}

	public ListIterator<XConfigValue> listIterator() {
		return unmodifiableView().listIterator();
	}

	public ListIterator<XConfigValue> listIterator(int index) {
		return unmodifiableView().listIterator(index);
	}

	public List<XConfigValue> subList(int fromIndex, int toIndex) {
		return unmodifiableView().subList(fromIndex, toIndex);
	}

	private List<XConfigValue> unmodifiableView() {
		List<XConfigValue> view = unmodifiableView;
		if (view == null) {
			view = Collections.unmodifiableList(values);
			unmodifiableView = view;
		}
		return view;
	}
}
