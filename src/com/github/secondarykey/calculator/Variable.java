package com.github.secondarykey.calculator;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 */
public class Variable {
	private Map<String,Object> values = new HashMap<>();
	public void add(String name,Object object) {
		values.put(name, object);
	}
	public Object get(String name) {
		return values.get(name);
	}
	public Set<String> getDefineList() {
		return values.keySet();
	}
}
