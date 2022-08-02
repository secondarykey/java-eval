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
		
		Object obj = cal.eval("if ( true ) false");
		assertTrue((Boolean)obj);
		
		
		
		

		String code = TestUtil.get("/text/if.txt");
		obj = cal.eval(code);

		assertEquals(rtn.size(),3,"戻り値のサイズ");
	}

}
