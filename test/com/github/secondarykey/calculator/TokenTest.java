package com.github.secondarykey.calculator;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import com.github.secondarykey.calculator.Token.Value;

class TokenTest {

	@Test
	void testValue() {
		
		assertEquals(Value.INVOKER.getLastIndex("$test.index()"),11);
	}

}
