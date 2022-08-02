package com.github.secondarykey.calculator;

public class Caluculator {
	
	public static Caluculator create(Variable var) {
		return new Caluculator(var);
	}

	private Variable var;
	public Caluculator(Variable var) {
		this.var = var;
	}

	public Object eval(String lines) {
		Expression exp = new Expression(lines);
		return exp.eval(var);
	}
}
