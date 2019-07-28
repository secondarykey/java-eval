package com.github.secondarykey.calculator;

import java.util.ArrayList;
import java.util.List;

import com.github.secondarykey.calculator.Token.Control;
import com.github.secondarykey.calculator.Token.Operator;
import com.github.secondarykey.calculator.Token.Type;
import com.github.secondarykey.calculator.Token.Value;

/**
 * 語句解析器
 * @author secon
 */
public class Parser {
	private int idx = 0;
	private List<Token> values;
	public Parser(List<Token> values) {
		this.values = new ArrayList<>(values);
		//終端を追加
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

	private void increment() {
		++idx;
	}

	public Token get(int priority) {
	
		Token left = lead(getToken());
		increment();

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
			right.SetLeft(left);
			int priority = right.getPriority();
			right.SetRight(get(priority-1));
			return right;
		} else {
			throw new ParseException("演算子の位置がおかしい" + right);
		}
	}

	private Token lead(Token token) {
		Type type = token.getType();
		if ( type instanceof Value ) {
			return token;
		} else if ( type == Operator.OPEN ) {
			Token left = get(0);
			search();
			return left;
		//} else if ( type == Operator.NOT ) {
			//return token;
		} else {
			throw new ParseException("左辺に予期しない値が存在します。" + token);
		}
	}
	
	private void search() {
		Token token = getToken();
		if ( token.getType() != Operator.CLOSE )  {
			throw new RuntimeException("閉じカッコの状態がおかしい" + token);
		}
		increment();
		return;
	}

	/**
	 * 構造解析の例外
	 * @author secon
	 */
	public class ParseException extends RuntimeException {
		private static final long serialVersionUID = 1L;
		public ParseException(String string) {
			super(string);
		}
	}
}
