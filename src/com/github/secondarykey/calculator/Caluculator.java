package com.github.secondarykey.calculator;

import java.util.logging.Logger;

/**
 * 計算機
 * <pre>
 * 変数を設定し、プログラムを評価
 * </pre>
 * @author secon
 */
public class Caluculator {
	
	@SuppressWarnings("unused")
	private static final Logger logger = Logger.getLogger(Caluculator.class.toString());

	/**
	 * 計算機生成
	 * @param var 対象変数
	 * @return 計算機
	 */
	public static Caluculator create(Variable var) {
		return new Caluculator(var);
	}

	/**
	 * 変数領域
	 */
	private Variable var;

	/**
	 * コンストラクタ
	 * @param var 変数
	 */
	private Caluculator(Variable var) {
		this.var = var;
	}

	/**
	 * 評価
	 * @param lines プログラム文字列
	 * @return 結果
	 */
	public Object eval(String lines) {
		Expression exp = new Expression(lines);
		return exp.eval(var);
	}
}
