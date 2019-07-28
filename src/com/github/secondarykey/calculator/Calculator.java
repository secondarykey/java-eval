package com.github.secondarykey.calculator;

import java.util.ArrayList;
import java.util.List;

/**
 * 計算機
 * <pre>
 * 変数の値と評価器を保持し、実行を行う
 * 連続実行し、真を持つインデックスを返す
 * </pre>
 * @author secon
 */
public class Calculator {

	private List<Expression> expList = new ArrayList<>();
	private Variable param = new Variable();
	public void addVariable(String name,Object value) {
		param.add(name, value);
	}
	
	public int eval() {
		for ( Expression exp : expList ) {
		}
		return -1;
	}

}
