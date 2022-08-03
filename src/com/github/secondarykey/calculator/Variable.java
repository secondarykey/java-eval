package com.github.secondarykey.calculator;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * 変数領域
 */
public class Variable {
	
	/**
	 * 大域変数
	 */
	private Map<String,Object> values = new HashMap<>();

	/**
	 * インタプリタ用領域
	 */
	private Map<String,Object> box = new HashMap<>();

	/**
	 * 大域変数追加
	 * @param name 
	 * @param object
	 */
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
