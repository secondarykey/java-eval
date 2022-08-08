package com.github.secondarykey.calculator;

import java.util.List;

/**
 * 構文木
 * @author secon
 */
public class Ast {

	/**
	 * 解析済みトークンリスト
	 */
	private List<Token> list;

	/**
	 * コンストラクタ
	 * @param list
	 */
	public Ast(List<Token> list) {
		this.list = list;
	}

	/**
	 * 解析済のトークンを取得
	 * @return トークンリスト
	 */
	public List<Token> get() {
		return list;
	}
}
