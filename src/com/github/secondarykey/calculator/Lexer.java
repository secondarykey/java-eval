package com.github.secondarykey.calculator;

import java.util.ArrayList;
import java.util.List;

import com.github.secondarykey.calculator.Token.Operator;
import com.github.secondarykey.calculator.Token.Type;
import com.github.secondarykey.calculator.Token.Value;

public class Lexer {

	private String value;

	private static List<Type> types = null;

	private static void init() {
		/**
		 * 比較用に作っておく
		 */
		if ( types == null ) {
			types = new ArrayList<Type>();
			for ( Operator op : Operator.values() ) {
				types.add(op);
			}
			for ( Value val : Value.values() ) {
				types.add(val);
			}
		}
		return;
	}

	public Lexer(String line) {
		init();
		value = line;
	}
	
	public List<Token> analysis() {

		List<Token> rtn = new ArrayList<Token>();
		String buf = skip(value);

		while ( !buf.isEmpty() ) {
			boolean notfound = true;
			for (Type t : types) {
            	int index = t.getLastIndex(buf);
            	if ( index != -1 ) {
            		int pre = 0;
            		int suf = index;
            		if ( t.equals(Value.VARIABLE) ) {
            			pre = 1;
            			
            			int wk = Value.INVOKER.getLastIndex(buf);
            			if ( wk != -1 ) {
            				index = wk;
            				suf = index;
            				t = Value.INVOKER;
            			}

            		} else if ( t.equals(Value.STRING) ) {
            			pre = 1;
            			suf = index - 1;
            		} else if ( t.equals(Operator.GT) ) {
            			int wk = Operator.GE.getLastIndex(buf);
            			if ( wk != -1 ) {
            				index = wk;
            				suf = index;
            				t = Operator.GE;
            			}
            		} else if ( t.equals(Operator.LT) ) {
            			int wk = Operator.LE.getLastIndex(buf);
            			if ( wk != -1 ) {
            				index = wk;
            				suf = index;
            				t = Operator.LE;
            			}
            		} else if ( t.equals(Operator.NOT) ) {
            			int wk = Operator.NE.getLastIndex(buf);
            			if ( wk != -1 ) {
            				index = wk;
            				suf = index;
            				t = Operator.NE;
            			}
            		} else if ( t.equals(Value.INTEGER) ) {
            			int wk = Value.REAL.getLastIndex(buf);
            			if ( wk != -1 ) {
            				index = wk;
            				t = Value.REAL;
            			}


            		}

            		String val = buf.substring(pre, suf);
            		Token token = new Token(t,val);
            		rtn.add(token);

            		buf = buf.substring(index);
            		notfound = false;
            		break;
            	}
        	}
			
			buf = skip(buf);
			if ( notfound ) {
				throw new LexerException("[" + buf  + "]");
			}
		}
		
        return rtn;
	}	
	

	private String skip(String buf) {
		return buf.trim();
	}	
	
	public class LexerException extends RuntimeException {
		private static final long serialVersionUID = 1L;
		public LexerException(String string) {
			super(string);
		}
	}
}
