package com.github.secondarykey.calculator;

import java.util.ArrayList;
import java.util.List;
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

	/**
	 * AST解析
	 * @param lex 字句解析器
	 * @return 構文木
	 */
	public static Ast parse(Lexer lex) {
		AstParser p = new AstParser(lex);
		List<Token> list = lex.getTokenList();
		logger.info("Parse Start ==============================");
		Ast ast = p.createAst(list);
		logger.info("Parse End   ==============================");
		return ast;
	}

	/**
	 * AST解析
	 * @param tokenList トークンリスト
	 * @return 構文木
	 */
	public static Ast parse(List<Token> tokenList) {
		AstParser p = new AstParser();
		return p.createAst(tokenList);
	}

	/**
	 * 字句解析器(デバッグ用)
	 */
	private Lexer lex = null;
	/**
	 * 字句リスト
	 */
	private List<Token> values;
	/**
	 * 字句インデックス
	 */
	private int idx = 0;
	/**
	 * 永久ループ処理用(基本的に使われる場合、エラーが起こっている)
	 */
	private int limit;

	/**
	 * コンストラクタ
	 */
	private AstParser() {
	}

	/**
	 * コンストラクタ(字句解析器)
	 * @param lex 字句解析器
	 */
	private AstParser(Lexer lex) {
		this.lex = lex;
	}

	/**
	 * 構文解析
	 * <pre>
	 * 字句解析リストから構文解析を行う
	 * ParserインスタンスがLexerから作成されている場合のみ全体のデバッグを表示可能
	 * </pre>
	 */
	private Ast createAst(List<Token> values) {

		limit = values.size();

		logger.fine("トークンサイズ"+limit);
		for ( Token token : values ) {
			logger.fine(token.getValue());
		}

		this.values = new ArrayList<>(values);
		this.values.add(new Token(Control.EOT,null));

		List<Token> rtn = new ArrayList<>();

		while( hasNext() ) {
			logger.fine("Parser loop get()");
			Token branch = get(0);

			logger.fine("PARSE BRANCH TOKEN:" + branch);
			//EOTの場合追加しない
			if ( !branch.isType(Control.EOT) && !branch.isType(Operator.CLOSE_BLOCK) &&
			     !branch.isType(Operator.SEMICOLON) ) {
				rtn.add(branch);
			}
		}
		
		return new Ast(rtn);
	}

	/**
	 * 次の字句を持っているか？
	 * @return 存在する場合true
	 */
	private boolean hasNext() {
		logger.fine("hasNext():" + idx);
		Token token = getToken();
		if ( token.isType(Control.EOT) ) {
			return false;
		}
		return true;
	}

	/**
	 * 処理時点でのトークンの取得
	 * @return 現在のトークン
	 */
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

	/**
	 * インデックス加算
	 */
	private void increment() {
		if ( values.size() != (idx + 1) ) {
			++idx;
		} else {
			limit--;
			if ( limit == 0 ) {
				throw new ParseException(this,getToken(),"Parseエラー:永久ループ解除用");
			}
		}
	}

	/**
	 * 要素の取得
	 * <pre>
	 * 現在の要素を取得し、右辺等の設定を行う
	 * </pre>
	 * @param priority 優先順位
	 * @return 解析したトークン
	 */
	private Token get(int priority) {

		Token n = getAndIncrement();
		logger.fine("Token:" + n);
		if ( n.isType(Control.EOT) ) {
			return n;
		}
		if ( n.isType(Operator.CLOSE_BLOCK) ) {
			return n;
		}
		if ( n.isType(Operator.SEMICOLON) ) {
			return n;
		}

		Token left = lead(n);
		//右辺がいらないパターンの場合
		if ( left.isNoneRight() ) {
			return left;
		}

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
	 * ただし、オペレーターでしか処理を行わない
	 * </pre>
	 * @param left 左辺
	 * @param right 右辺
	 * @return
	 */
	private Token bind(Token left, Token right ) {
	
		if ( right.getType() instanceof Operator ) {
			right.setLeft(left);
			int priority = right.getPriority();
			right.setRight(get(priority-1));
			return right;
		} else {
			throw new ParseException(this,right,"オペレーター以外でのバインドが存在");
		}
	}

	/**
	 * 先頭の取得
	 * @param token 対象トークン
	 * @return トークンを返す
	 */
	private Token lead(Token token) {
	
		logger.fine(token.toString());
		
		Type type = token.getType();

		if ( type instanceof Value ) {
			//呼び出し処理の場合
			if ( type.equals(Value.INVOKER) ) {

				//TODO 複数引数の処理
				Token next = getAndIncrement();
				if ( !next.isType(Operator.OPEN) ) {
					throw new ParseException(this,next,"関数呼出がカッコから始まっていません。");
				}

				Token args = getToken();
				token.setBlocks(arguments(args));
				
				increment();
				return token;
			} else if ( type.equals(Value.IDENTIFIER) ) {

				String val = token.getValue();
				// if 文の場合
				if ( val.equals("if") ) {

					Token next = getAndIncrement();
					if ( !next.isType(Operator.OPEN) ) {
						throw new ParseException(this,next,"if文がカッコから始まっていません。");
					}
		
					//内部式を右辺に設定しておく
					Token exp = get(0);
					token.setRight(exp);
	
					Token close = getAndIncrement();
					if ( !close.isType(Operator.CLOSE) ) {
						throw new ParseException(this,close,"if文が閉じられていません。");
					}

					Token op = getToken();
					//中括弧を確認
					token.setBlocks(blocks(op));

					logger.fine("return " + token);
					//if文を返す
					return token;

				} else if ( val.equals("return") ) {
					//内部式を追加
					Token exp = get(0);
					token.setRight(exp);
					return token;
				} else if ( val.equals("let") ) {
		
					//名称を設定しておく
					Token name = getToken();
					token.setRight(name);

					//次がアサイン
					increment();

					Token assign = getToken();
					if ( !assign.isType(Operator.ASSIGN) ) {
						throw new ParseException(this,assign,"let式に代入(=)が存在しません。");
					}

					increment();
					//右辺を設定して終了
					Token right = get(0);
					name.setRight(right);

					return token;
				}
			}
			return token;
		} else if ( type.equals(Operator.NOT) ) {
			Token val = get(token.getPriority());
			token.setRight(val);
			return token;
		} else if ( type.equals(Operator.OPEN) ) {
			Token left = get(0);
			checkClose();
			return left;
		} else if ( type.equals(Operator.SEMICOLON) ) {
			return token;
		} else if ( type.equals(Operator.CLOSE_BLOCK)  ||   
		            type.equals(Control.EOT) ) {
			logger.fine("Controller:" + token);
			return token;
		} else {
			throw new ParseException(this,token,"lead()時の例外:");
		}
	}

	/**
	 * 関数用の引数を作成
	 * @param args 先頭のトークン
	 * @return 関数部分の引数のトークンリスト
	 */
	private List<Token> arguments(Token args) {

		List<Token> tokens = new ArrayList<>();
		if ( args.isType(Operator.CLOSE) ) {
			return tokens;
		}

		Token target = args;
		while ( true ) {
			Token work = getToken();
			if ( work.isType(Operator.COMMA) || work.isType(Operator.CLOSE) ) {
				tokens.add(target);
				if ( work.isType(Operator.CLOSE) ) {
					break;
				}
			}
			target = work;
			increment();
		}
		return tokens;
	}

	/**
	 * ブロックを作成
	 * <pre>
	 * {} で囲んだ位置を実行用に作成
	 * </pre>
	 * @param op 開始位置
	 * @return 内部のTokenリスト
	 */
	private List<Token> blocks(Token op) {
		
		logger.fine("blocks()");

		if ( !op.isType(Operator.OPEN_BLOCK) ) {
			throw new ParseException(this,op,"if文のブロックがありません");
		}

		increment();
		List<Token> blocks = new ArrayList<>();

		while ( true ) {
			Token n = get(0);
			logger.fine("block:" + n);
			if ( n.isType(Operator.CLOSE_BLOCK) ) {
				break;
			}
			increment();
			blocks.add(n);
		}

		logger.fine("block End");
		return blocks;
	}

	/**
	 * 閉じカッコのチェック 
	 */
	private void checkClose() {
		Token token = getToken();
		if ( !token.isType(Operator.CLOSE) )  {
			throw new ParseException(this,token,"閉じカッコがおかしい");
		}
		increment();
		return;
	}

	/**
	 * 字句解析器の取得(デバッグ用)
	 * @return 字句解析器
	 */
	private Lexer getLexer() {
		return lex;
	}

	/**
	 * 解析時例外
	 * <pre>
	 * 渡されたパーサーの字句解析器により、エラー位置を特定し表示できるようにする
	 * ただしパーサーがLexerで作成されてない場合はエラーメッセージのみとなる。
	 * </pre>
	 */
	public class ParseException extends RuntimeException {

		private static final long serialVersionUID = 1L;
		/**
		 * 字句解析器
		 */
		private Lexer lex;
		/**
		 * 対象トークン
		 */
		private Token token;

		/**
		 * 解析時例外コンストラクタ
		 * @param p 対象パーサー
		 * @param token 対象トークン
		 * @param string 対象文字列
		 */
		public ParseException(AstParser p,Token token,String string) {
			super(string);
			this.lex = p.getLexer();
			this.token = token;
		}

		@Override
		public String getMessage() {
			String msg = super.getMessage();
			if ( lex != null ) {
				msg = lex.debugLine(this.token,msg);
			}
			return msg;
		}
	}

}
