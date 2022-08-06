package com.github.secondarykey.calculator;

import java.util.List;

public class Ast {

	private List<Token> list;

	public Ast(List<Token> list) {
		this.list = list;
	}

	public List<Token> get() {
		return list;
	}
}
