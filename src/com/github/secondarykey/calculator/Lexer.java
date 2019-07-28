package com.github.secondarykey.calculator;

import java.util.ArrayList;
import java.util.List;

import com.github.secondarykey.calculator.Token.Operator;
import com.github.secondarykey.calculator.Token.Type;
import com.github.secondarykey.calculator.Token.Value;

/**
 * 字句解析器
 * @author secon
 */
public class Lexer {

	private String value;

	private static List<Type> types = null;

	private static void init() {
		//定義の読み込み
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
	
	/**
	 * 字句解析
	 * <pre>
	 * 演算子等の塊に変更する
	 * </pre>
	 * @return トークンのリスト
	 */
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
            			//$を削除
            			pre = 1;
            		} else if ( t.equals(Value.STRING) ) {
            			//DQを削除
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
				throw new LexerException("字句解析時に解析不可能な字句が存在しました。[" + buf  + "]");
			}
		}
		
		//終端を入れて返す
        return rtn;
	}	
	

	/**
	 * 字句解析スキップ
	 * @param buf
	 * @return
	 */
	private String skip(String buf) {
		//現状、空白だけ
		return buf.trim();
	}	
	
	/**
	 * 字句解析の例外
	 * @author secon
	 */
	public class LexerException extends RuntimeException {
		private static final long serialVersionUID = 1L;
		public LexerException(String string) {
			super(string);
		}
	}
}
