package com.github.secondarykey.calculator;

import java.util.ArrayList;
import java.util.List;

import com.github.secondarykey.calculator.Token.Control;
import com.github.secondarykey.calculator.Token.Operator;
import com.github.secondarykey.calculator.Token.Type;
import com.github.secondarykey.calculator.Token.Value;

/**
 * 構文解析器
 * <pre>
 * </pre>
 */
public class Parser {

	private int idx = 0;

	private List<Token> values;

	public Parser(List<Token> values) {
		this.values = new ArrayList<>(values);
		this.values.add(new Token(Control.EOT,null));
	}

	public boolean hasNext() {
		Token token = values.get(idx);
		if ( token.getType().equals(Control.EOT) ) {
			return false;
		}
		return true;
	}

	private Token getToken() {
		return values.get(idx);
	}

	private Token next() {
		Token rtn = getToken();
		increment();
		return rtn;
	}

	private void increment() {
		++idx;
	}

	public Token get(int priority) {
	
		Token left = lead(next());
		Token right = getToken();

		while ( priority < right.getPriority() ) {
			increment();
			left = bind(left,right);
			right = getToken();
		}

		return left;
	}

	private Token bind(Token left, Token right) {
	
		if ( right.getType() instanceof Operator ) {
			right.setLeft(left);
			int priority = right.getPriority();
			right.setRight(get(priority-1));
			return right;
		} else {
			throw new ParseException("オペレーター以外でのバインドが存在" + right);
		}
	}

	private Token lead(Token token) {
		Type type = token.getType();
		if ( type instanceof Value ) {
			if ( type == Value.INVOKER ) {
				Token val = next();
				if ( val.getType() != Operator.OPEN ) {
					throw new ParseException("関数呼び出しタイプの書き方に不正があります。" + val);
				}

				Token close = getToken();
				if ( close.getType() != Operator.CLOSE ) {
					token.setRight(close);
					increment();
				} else {
					token.setRight(new Token(Control.NOPARAM,null));
				}
				increment();
				return token;
			}
			return token;
		} else if ( type == Operator.OPEN ) {
			Token left = get(0);
			checkClose();
			return left;
		} else if ( type == Operator.NOT ) {
			Token val = get(token.getPriority());
			token.setRight(val);
			return token;
		} else {
			throw new ParseException("lead時の例外" + token);
		}
	}
	
	private void checkClose() {
		Token token = getToken();
		if ( token.getType() != Operator.CLOSE )  {
			throw new RuntimeException("" + token);
		}
		increment();
		return;
	}

	/**
	 */
	public class ParseException extends RuntimeException {
		private static final long serialVersionUID = 1L;
		public ParseException(String string) {
			super(string);
		}
	}
}
