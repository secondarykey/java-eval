package com.github.secondarykey.calculator;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;

class CaluculatorTest {

	@Test
	void testIF() {
		//変数値
		Variable var = new Variable();
		List<String> rtn = new ArrayList<>();
		var.add("rtn", rtn);
		var.add("flag", true);

		Caluculator cal = Caluculator.create(var);

		String code = TestUtil.get("/text/if.txt");
		cal.eval(code);

		assertEquals(rtn.size(),3,"戻り値のサイズ");

		rtn = new ArrayList<>();
		var.add("rtn", rtn);
		var.add("flag", false);
		//if文を通らないことを確認
		cal.eval(code);
		assertEquals(rtn.size(),2,"戻り値のサイズ");
	}
	
	@Test 
	void testLong() {
		Variable var = new Variable();
		List<String> list = new ArrayList<>();
		var.add("list", list);
		var.add("flag", true);

		Caluculator cal = Caluculator.create(var);

		String code = TestUtil.get("/text/long.txt");
		Object rtn = cal.eval(code);
		assertEquals(rtn,"OK!");
	
		list = new ArrayList<>();
		var.add("list", list);
		var.add("flag", false);
		
		rtn = cal.eval(code);
		assertEquals(rtn,"NG");
	}
	
	@Test 
	void testLet() {

		Variable var = new Variable();
		var.add("var", "sample");

		Caluculator cal = Caluculator.create(var);

		String code = TestUtil.get("/text/let.txt");
		Object rtn = cal.eval(code);
		assertEquals(rtn,0);
	
		var.add("var","test");
		
		rtn = cal.eval(code);
		assertEquals(rtn,1);
	}
}
