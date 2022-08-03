package com.github.secondarykey.calculator;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.github.secondarykey.calculator.Token.Control;
import com.github.secondarykey.calculator.Token.Operator;
import com.github.secondarykey.calculator.Token.Type;
import com.github.secondarykey.calculator.Token.Value;

/**
 * 構文解析器
 * <pre>
 * </pre>
 */
public class AstParser {
	
	@SuppressWarnings("unused")
	public static final Logger logger = Logger.getLogger(AstParser.class.getName());
	
	static {
        logger.setLevel(Level.WARNING);
        System.setProperty("java.util.logging.SimpleFormatter.format",
               "%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS.%1$tL %4$s %2$s %5$s%6$s%n");
	}
	
	public static Ast parse(List<Token> tokens) {
		logger.info(logger.getLevel().toString());
		logger.info("--------------------------- Parse");
		AstParser p = new AstParser();
		return p.createAst(tokens);
	}

	private int idx = 0;
	private List<Token> values;
	private int limit;

	private AstParser() {
	}

	/**
	 * 構文解析
	 * <pre>
	 * 字句解析リストから構文解析を行う
	 * </pre>
	 */
	private Ast createAst(List<Token> values) {

		limit = values.size();

		logger.info("トークンサイズ"+limit);
		for ( Token token : values ) {
			logger.info(token.getValue());
		}

		this.values = new ArrayList<>(values);
		this.values.add(new Token(Control.EOT,null));

		List<Token> rtn = new ArrayList<>();

		while( hasNext() ) {
			logger.info("Parser loop get()");
			Token branch = get(0,0);

			logger.info("PARSE BRANCH TOKEN:" + branch);
			//EOTの場合追加しない
			if ( !branch.isType(Control.EOT) && !branch.isType(Operator.CLOSE_BLOCK) &&
			     !branch.isType(Operator.SEMICOLON) ) {
				rtn.add(branch);
			}
		}
		
		return new Ast(rtn);
	}

	private boolean hasNext() {
		logger.info("hasNext():" + idx);
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
	private Token getAndIncrement() {
		Token rtn = getToken();
		increment();
		return rtn;
	}

	private void increment() {
		logger.info("increment():" + idx);
		if ( values.size() != (idx + 1) ) {
			++idx;
		} else {
			limit--;
			if ( limit == 0 ) {
				throw new ParseException("Parseエラー:永久ループ解除用");
			}
		}
	}

	/**
	 * 要素の取得
	 * @param priority
	 * @return
	 */
	private Token get(int priority,int depth) {
		
		logger.info(":" + depth);

		Token n = getAndIncrement();
		logger.info("Token:" + n);
		if ( n.isType(Control.EOT) ) {
			return n;
		}
		if ( n.isType(Operator.CLOSE_BLOCK) ) {
			return n;
		}
		if ( n.isType(Operator.SEMICOLON) ) {
			return n;
		}

		Token left = lead(n,depth);
		//右辺がいらないパターンの場合
		if ( left.isNoneRight() ) {
			return left;
		}

		Token right = getToken();
		while ( priority < right.getPriority() ) {
			increment();
			left = bind(left,right,depth);
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
	private Token bind(Token left, Token right,int depth) {
	
		if ( right.getType() instanceof Operator ) {
			right.setLeft(left);
			int priority = right.getPriority();
			right.setRight(get(priority-1,depth));
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
	private Token lead(Token token,int depth) {
	
		logger.info(depth + ":" + token);
		
		Type type = token.getType();

		if ( type instanceof Value ) {
			//呼び出し処理の場合
			if ( type.equals(Value.INVOKER) ) {
				//TODO 複数引数の処理
				Token next = getAndIncrement();
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

					Token next = getAndIncrement();
					if ( !next.isType(Operator.OPEN) ) {
						throw new ParseException("if文がカッコから始まっていません。" + val);
					}
		
					//内部式を右辺に設定しておく
					Token exp = lead(next,depth);
					token.setRight(exp);

					Token op = getToken();
					//中括弧を確認
					token.setBlocks(blocks(op,depth));

					logger.info(depth + ":return " + token);
					//if文を返す
					return token;

				} else if ( val.equals("return") ) {
					//内部式を追加
					Token exp = get(0,depth);
					token.setRight(exp);
					return token;
				}
			}
			return token;
		} else if ( type.equals(Operator.NOT) ) {
			Token val = get(token.getPriority(),depth);
			token.setRight(val);
			return token;
		} else if ( type.equals(Operator.OPEN) ) {
			Token left = get(0,depth);
			checkClose();
			return left;
		} else if ( type.equals(Operator.SEMICOLON) ) {
			return token;
		} else if ( type.equals(Operator.CLOSE_BLOCK)  ||   
		            type.equals(Control.EOT) ) {
			logger.info("Controller:" + token);
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
	private List<Token> blocks(Token op,int depth) {
		
		logger.info("blocks()");

		if ( !op.isType(Operator.OPEN_BLOCK) ) {
			throw new ParseException("if文のブロックがありません" + op);
		}

		increment();
		List<Token> blocks = new ArrayList<>();

		while ( true ) {
			Token n = get(0,depth + 1);
			logger.info("block:" + n);
			if ( n.isType(Operator.CLOSE_BLOCK) ) {
				break;
			}
			increment();
			blocks.add(n);
		}

		logger.info("block End");
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