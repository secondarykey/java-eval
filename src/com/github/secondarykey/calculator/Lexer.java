package com.github.secondarykey.calculator;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import com.github.secondarykey.calculator.Token.Operator;
import com.github.secondarykey.calculator.Token.Type;
import com.github.secondarykey.calculator.Token.Value;

/**
 * 字句解析木
 * @author secon
 *
 */
public class Lexer {

	@SuppressWarnings("unused")
	private static final Logger logger = Logger.getLogger(Lexer.class.getName());

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

           		//TODO 単項演算子(+,-)

            	//存在した場合
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
            			// GEと解析を間違う場合がある為、処理を行う
            			int wk = Operator.GE.getLastIndex(buf);
            			if ( wk != -1 ) {
            				index = wk;
            				suf = index;
            				t = Operator.GE;
            			}
            		} else if ( t.equals(Operator.LT) ) {
            			// LEと解析を間違う場合がある為、処理を行う
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
            				suf = index;
            				t = Value.REAL;
            			}
            		} else if ( t.equals(Operator.ASSIGN) ) {
            			int wk = Operator.EQ.getLastIndex(buf);
            			if ( wk != -1 ) {
            				index = wk;
            				suf = index;
            				t = Operator.EQ;
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

	/**
	 * 字句解析例外
	 * @author secon
	 */
	public class LexerException extends RuntimeException {
		private static final long serialVersionUID = 1L;
		public LexerException(String string) {
			super(string);
		}
	}
}
