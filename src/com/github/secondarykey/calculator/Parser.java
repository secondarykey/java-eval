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

	public Parser() {
	}

	/**
	 * 構文解析
	 * <pre>
	 * 字句解析リストから構文解析を行う
	 * </pre>
	 */
	public List<Token> parse(List<Token> values) {

		this.values = new ArrayList<>(values);
		this.values.add(new Token(Control.EOT,null));

		List<Token> rtn = new ArrayList<>();
		while( hasNext() ) {
			Token branch = get(0);
			//EOTの場合追加しない
			if ( !branch.isType(Control.EOT) ) {
				rtn.add(branch);
			}
		}
		return rtn;
	}

	private boolean hasNext() {
		Token token = values.get(idx);
		if ( token.isType(Control.EOT) ) {
			return false;
		}
		return true;
	}

	private Token getToken() {
		return values.get(idx);
	}

	/**
	 * 現在のトークンを取得しインクリメントして終了
	 * @return 現在のトークン
	 */
	private Token nowAndIncrement() {
		Token rtn = getToken();
		increment();
		return rtn;
	}

	private void increment() {
		if ( values.size() != idx + 1 ) {
			++idx;
		} else {
			System.out.println("error");
		}
	}

	private Token get(int priority) {
		
		Token n = nowAndIncrement();
		if ( n.isType(Operator.SEMICOLON) ) {
			n = nowAndIncrement();
		}

		if ( n.isType(Control.EOT) ) {
			return n;
		}

		Token left = lead(n);

		Token right = getToken();
		while ( priority < right.getPriority() ) {
			increment();
			left = bind(left,right);
			right = getToken();
		}
		return left;
	}

	/**
	 * 左辺右辺を代入
	 * <pre>
	 * 優先順位の状況で設定を行う
	 * </pre>
	 * @param left
	 * @param right
	 * @return
	 */
	private Token bind(Token left, Token right) {
	
		if ( right.getType() instanceof Operator ) {
			right.setLeft(left);
			int priority = right.getPriority();
			right.setRight(get(priority-1));
			return right;
		} else {
			throw new ParseException("オペレーター以外でのバインドが存在:" + right);
		}
	}

	/**
	 * 先頭の取得
	 * @param token 対象トークン
	 * @return トークンを返す
	 */
	private Token lead(Token token) {
		Type type = token.getType();

		if ( type instanceof Value ) {
			//呼び出し処理の場合
			if ( type.equals(Value.INVOKER) ) {
				//TODO 複数引数の処理
				Token next = nowAndIncrement();
				if ( !next.getType().equals(Operator.OPEN) ) {
					throw new ParseException("関数呼出がカッコから始まっていません。" + next);
				}

				Token close = getToken();
				if ( !close.isType(Operator.CLOSE) ) {
					//TODO 引数を複数解析
					token.setRight(close);
					increment();
				} else {
					token.setRight(new Token(Control.NOPARAM,null));
				}
				increment();
				return token;
			} else if ( type.equals(Value.IDENTIFIER) ) {
				String val = token.getValue();
				// if 文の場合
				if ( val.equals("if") ) {

					Token next = nowAndIncrement();
					if ( !next.isType(Operator.OPEN) ) {
						throw new ParseException("if文がカッコから始まっていません。" + val);
					}
		
					//内部式を追加
					Token exp = get(0);

					token.setRight(exp);
					increment();

					Token op = getToken();
					//中括弧を確認
					token.setBlocks(blocks(op));

					return token;
				} else if ( val.equals("return") ) {
					//内部式を追加
					Token exp = get(0);
					token.setRight(exp);
					return token;
				}
			}
			return token;
		} else if ( type.equals(Operator.OPEN) ) {
			Token left = get(0);
			checkClose();
			return left;
		} else if ( type.equals(Operator.CLOSE_BLOCK) ) {
			return token;
		} else if ( type.equals(Operator.NOT) ) {
			Token val = get(token.getPriority());
			token.setRight(val);
			return token;
		} else if ( type.equals(Operator.SEMICOLON) ) {
			increment();
			return token;
		} else if ( type.equals(Control.EOT) ) {
			return token;
		} else {
			throw new ParseException("lead()時の例外:" + token);
		}
	}

	/**
	 * ブロックを作成
	 * <pre>
	 * 
	 * </pre>
	 * @param op 開始位置
	 * @return 内部のTokenリスト
	 */
	private List<Token> blocks(Token op) {

		if ( !op.isType(Operator.OPEN_BLOCK) ) {
			throw new ParseException("if文のブロックがありません" + op);
		}

		increment();
		List<Token> blocks = new ArrayList<>();

		while ( true ) {
			Token n = get(0);
			if ( n.isType(Operator.CLOSE_BLOCK) ) {
				break;
			}
			blocks.add(n);
		}

		increment();
		return blocks;
	}

	private void checkClose() {
		Token token = getToken();
		if ( !token.isType(Operator.CLOSE) )  {
			throw new RuntimeException("閉じカッコがおかしい" + token);
		}
		increment();
		return;
	}

	/**
	 * 解析時例外
	 */
	public class ParseException extends RuntimeException {
		private static final long serialVersionUID = 1L;
		public ParseException(String string) {
			super(string);
		}
	}
}
