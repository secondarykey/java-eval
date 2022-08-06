package com.github.secondarykey.calculator;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.util.logging.LogManager;

import org.junit.jupiter.api.Test;

import com.github.secondarykey.calculator.Token.Value;

class TokenTest {
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
	void testValue() {
		
		assertEquals(Value.INVOKER.getLastIndex("test.index();test"),10);
		assertEquals(Value.INVOKER.getLastIndex("test.index(\"test\")"),10);
		assertEquals(Value.INVOKER.getLastIndex("test.index(test)"),10);
		assertEquals(Value.INVOKER.getLastIndex("test.index(\"test\",test)"),10);
	}

}
