package com.github.secondarykey.calculator;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

/**
 * 真偽評価器
 * <pre>
 * True/FalseのExpressionをため込んで、引数で実行し、式のインデックス(順序)を取得する
 * </pre>
 */
public class BooleanCalculator {

	@SuppressWarnings("unused")
	public static final Logger logger = Logger.getLogger(BooleanCalculator.class.getName());
	
	//式リスト
	private List<Expression> expList = new ArrayList<>();
	public void addExpression(Expression exp) {
		expList.add(exp);
	}

	/* 引数リスト */
	private Variable param = new Variable();
	public void addVariable(String name,Object value) {
		param.add(name, value);
	}
	
	public void compile() {
		// 引数が網羅されているか？
	}

	/**
	 *
     * 式(1行)の一覧を評価し、trueの時、
     * 計算式のインデックスを返します。 
	 * @return
	 */
	public int eval() {
		//計算式数回繰り返す
		for ( int idx = 0; idx < expList.size(); ++idx ) {
			Object obj = expList.get(idx).eval(param);
			if ( !( obj instanceof Boolean ) ) {
				//式がBoolean型以外を返しています。
			} else {
				return idx;
			}
		}
		return -1;
	}
}
