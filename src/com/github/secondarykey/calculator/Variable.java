package com.github.secondarykey.calculator;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

/**
 * 変数領域
 */
public class Variable {

	@SuppressWarnings("unused")
	public static final Logger logger = Logger.getLogger(Variable.class.getName());	
	/**
	 * 大域変数
	 */
	private Map<String,Object> values = new HashMap<>();

	/**
	 * ローカル変数
	 */
	private Map<String,Object> local = new HashMap<>();

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

	public void addLocal(String name, Object val) {
		local.put(name,val);
	}

	public Object getLocal(String name) {
		return local.get(name);
	}
}
