package com.github.secondarykey.calculator;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.*;

import com.github.secondarykey.calculator.Expression;
import com.github.secondarykey.calculator.Variable;


class ExpressionTest {

	@Test
	void testIntValue() {

		Variable var = new Variable();
		var.add("value", 1);
		Expression eq = new Expression("$value == 1");
		assertTrue((Boolean)eq.eval(var));
		Expression ne = new Expression("$value != 1");
		assertFalse((Boolean)ne.eval(var));
		var.add("value", 2);
		assertFalse((Boolean)eq.eval(var));
		assertTrue((Boolean)ne.eval(var));
	}

	@Test
	void testStringValue() {

		Variable var = new Variable();
		var.add("value", "Test");

		Expression stringEq = new Expression("$value == \"Test\"");
		Expression stringNe = new Expression("$value != \"Test\"");
		assertTrue((Boolean)stringEq.eval(var));
		var.add("value", "Test1");
		assertFalse((Boolean)stringEq.eval(var));

		var.add("value", "Test");
		assertFalse((Boolean)stringNe.eval(var));
		var.add("value", "Test1");
		assertTrue((Boolean)stringNe.eval(var));
	}

	void testBoolValue() {
		Expression three1 = new Expression("1 == 1 && 2 == 2");
		assertTrue((Boolean)three1.eval(null));
		Expression three2 = new Expression("1 == 2 || 2 == 2");
		assertTrue((Boolean)three2.eval(null));

		Variable var = new Variable();
		var.add("value", true);
		Expression v = new Expression("$value");
		assertTrue((Boolean)v.eval(var));
		var.add("value", false);
		assertFalse((Boolean)v.eval(var));
	}

	void testOpenClose() {
		Expression three1 = new Expression("(1 == 1)");
		assertTrue((Boolean)three1.eval(null));
		Expression three2 = new Expression("(1 == 2 || (2 == 2))");
		assertTrue((Boolean)three2.eval(null));
	}	

		
		
		
}
