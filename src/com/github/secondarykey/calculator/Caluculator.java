package com.github.secondarykey.calculator;

import java.util.logging.Logger;

public class Caluculator {
	
	
	@SuppressWarnings("unused")
	private static final Logger logger = Logger.getLogger(Caluculator.class.toString());

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
