package com.github.secondarykey.calculator;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.LogManager;

import org.junit.jupiter.api.*;

class ExpressionTest {
	static {
	    try {
			LogManager.getLogManager().readConfiguration(
			        Caluculator.class.getResourceAsStream("/logging.properties"));
		} catch (SecurityException | IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}	
	}

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
	void testRealValue() {
		Variable var = new Variable();
		var.add("value", 2.5);
		Expression gt = new Expression("$value > 1.0");
		assertTrue((Boolean)gt.eval(var));

		Expression lt = new Expression("$value < 1.0");
		assertFalse((Boolean)lt.eval(var));

		var.add("value", 0.5);
		assertFalse((Boolean)gt.eval(var));
		assertTrue((Boolean)lt.eval(var));
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

	@Test
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

	@Test
	void testOpenClose() {
		Expression three1 = new Expression("(1 == 1)");
		assertTrue((Boolean)three1.eval(null));
		Expression three2 = new Expression("(1 == 2 || (2 == 2))");
		assertTrue((Boolean)three2.eval(null));
	}	


	@Test
	void testIdentity() {
		Expression iden1 = new Expression("true");
		assertTrue((Boolean)iden1.eval(null));
		Expression iden2 = new Expression("false");
		assertFalse((Boolean)iden2.eval(null));
		Expression iden3 = new Expression("null");
		assertNull(iden3.eval(null));
	}	
		
	@Test
	void testNot() {
		Expression three1 = new Expression("!(1 == 1)");
		assertFalse((Boolean)three1.eval(null));
		Expression three2 = new Expression("!(1 == 2 || (2 == 2))");
		assertFalse((Boolean)three2.eval(null));
		Expression three3 = new Expression("!(false || !false)");
		assertFalse((Boolean)three3.eval(null));
		Expression three4 = new Expression("!false && !false");
		assertTrue((Boolean)three4.eval(null));
	}	

	@Test
	void testCalc() {
		Expression plus = new Expression("1 + 1");
		assertEquals((Integer)plus.eval(null),2);
		Expression minus = new Expression("1 - 1");
		assertEquals((Integer)minus.eval(null),0);
		Expression mul = new Expression("5 * 2");
		assertEquals((Integer)mul.eval(null),10);
		Expression div = new Expression("6 / 2");
		assertEquals((Integer)div.eval(null),3);
		Expression mod = new Expression("5 % 3");
		assertEquals((Integer)mod.eval(null),2);

		plus = new Expression("1.2 + 1.3");
		assertEquals((Double)plus.eval(null),2.5,3);
		minus = new Expression("1.4 - 1.0");
		assertEquals((Double)minus.eval(null),0.4,3);
		mul = new Expression("1.1 * 5.0");
		assertEquals((Double)mul.eval(null),5.5,3);
		div = new Expression("6.0 / 2.2");
		assertEquals((Double)div.eval(null),3,3);
		mod = new Expression("5.2 % 3.1");
		assertEquals((Double)mod.eval(null),2,3);
	}

	@Test
	void testInvoke() {
		Variable var = new Variable();
		List<String> list = new ArrayList<>();
		list.add("test");
		var.add("value", list);
		
		Expression invoke1 = new Expression("$value.size() == 1");
		assertTrue((Boolean)invoke1.eval(var));

		Expression invoke2 = new Expression("$value.contains(" + "\"test\"" +  ")");
		assertTrue((Boolean)invoke2.eval(var));
	
		Expression invoke3 = new Expression("$value.contains(" + "\"other\"" +  ")");
		assertFalse((Boolean)invoke3.eval(var));
	}	
	
	@Test
	void testMultiLine() {
		Variable var = new Variable();
		List<String> list = new ArrayList<>();
		var.add("rtn", list);
		Expression prog = new Expression("$rtn.add(\"aaa\");$rtn.add(\"bbb\");");
		Object rtn = prog.eval(var);

		assertEquals(list.size(),2);
		assertNull(rtn);
	}

	@Test
	void testReturn() {
		Expression ret1 = new Expression("return \"test\"");
		assertEquals((String)ret1.eval(null),"test");
		Expression ret2 = new Expression("return true");
		assertTrue((Boolean)ret2.eval(null));
		Expression ret3 = new Expression("return false");
		assertFalse((Boolean)ret3.eval(null));
		Expression ret4 = new Expression("return 1+2");
		assertEquals((Integer)ret4.eval(null),3);
	}	
}
