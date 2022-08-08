package com.github.secondarykey.calculator;

import java.util.List;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * トークン用のオブジェクト
 */
public class Token {

	@SuppressWarnings("unused")
	public static final Logger logger = Logger.getLogger(Token.class.getName());	
	/**
	 * トークンの種別
	 */
	private Type type;
	/**
	 * トークンの値
	 */
	private String value;

	/**
	 * 対象の左辺
	 */
	private Token left;
	/**
	 * 対象の右辺
	 */
	private Token right;

	/**
	 * ブロック時の下リスト
	 */
	private List<Token> blocks;

	/**
	 * トークンを取り出した文字列上の位置(デバッグ用)
	 */
	private int position;

	/**
	 * コンストラクタ
	 * @param type
	 * @param val
	 */
	public Token(Type type, String val) {
		this.type = type;
		this.value = val;
	}

	/**
	 * トークンタイプの取得
	 * @return タイプ
	 */
	public Type getType() {
		return type;
	}

	/**
	 * 値の取得
	 * @return
	 */
	public String getValue() {
		return value;
	}

	/**
	 * 左辺の取得
	 * @return 左辺
	 */
	public Token left() {
		return left;
	}
	/**
	 * 右辺の取得
	 * @return 右辺
	 */
	public Token right() {
		return right;
	}

	/**
	 * 左辺の設定
	 * @param token
	 */
	public void setLeft(Token token) {
		this.left = token;
	}

	/**
	 * 右辺の設定
	 * @param token
	 */
	public void setRight(Token token) {
		this.right = token;
	}	

	/**
	 * 優先順位の取得
	 * @return 優先順位の取得
	 */
	public int getPriority() {
		if ( type instanceof Operator ) {
			return ((Operator)type).getPriority();
		} else if ( type.equals(Control.EOT) ) {
			return -1;
		} else if ( type.equals(Control.SEMICOLON) ) {
			return -1;
		}
		throw new RuntimeException("優先順位は存在しないはず" + this);
	}

	/**
	 * トークン文字列(タイプと値)
	 */
	public String toString() {
		return String.format("%-15s->%s",type,value);
	}

	/**
	 * タイプ判定
	 * @param type 判定タイプ
	 * @return そのタイプだった場合true
	 */
	public boolean isType(Type type) {
		return type.equals(this.type);
	}

	/**
	 * ブロックの設定
	 * @param blocks ブロック
	 */
	public void setBlocks(List<Token> blocks) {
		this.blocks = blocks;
	}

	/**
	 * ブロックの取得
	 * @return ブロック
	 */
	public List<Token> getBlocks() {
		return blocks;
	}

	/**
	 * 右辺を利用するか？
	 * <pre>
	 * ブロックが設定されているトークンは右辺設定を行っていない為、
	 * それを判定に利用
	 * </pre>
	 * @return 利用しない場合true
	 */
	public boolean isNoneRight() {
		if ( blocks != null ) {
			return true;
		}
		return false;
	}

	/**
	 * 自身の位置を設定（字句解析時のみ利用）
	 * @param position 位置（文字列に対する絶対的な位置）
	 */
	public void setPosition(int position) {
		this.position = position;
	}

	/**
	 * 自身の位置を取得
	 * @return 位置(文字数)
	 */
	public int getPosition() {
		return this.position;
	}	
	
	/**
	 * トークン種別
	 * <pre>
	 * 自身の字句のインデックス値(終端位置)を取得可能な状態にしておく
	 * </pre>
	 */
	public interface Type {
		int getLastIndex(String val);
	}

	/**
	 * 制御系のデータ
	 * @author secon
	 */
	public enum Control implements Type {

		SEMICOLON(";"),
		COMMA(","),
		COMMENT("//"),
		/**
		 * 字句エラー時に利用
		 */
		LEXER_ERROR,
		/**
		 * 構文解析時に最後に追加
		 */
		EOT;
	
		private String value;

		Control() {
		}

		Control(String val) {
			this.value = val;
		}
		

		@Override
		public int getLastIndex(String val) {

			if ( this.value == null ) {
				return -1;
			}

			int index = val.indexOf(this.value);
			if ( index == 0 ) {
				if ( this.equals(COMMENT) ) {
					index = val.indexOf("\n");
					if ( index != -1 ) {
						return val.length();
					}
					//最後までコメントとする
					return index;
				}
				return this.value.length();
			}
			return -1;

		}
	}

	/**
	 * 演算子
	 * @author secon
	 */
	public enum Operator implements Type {

		PLUS("+",70),
		MINUS("-",70),
		MUL("*",80),
		DIV("/",80),
		MOD("%",80),

		ASSIGN("=",50),

		OPEN("(",100),
		CLOSE(")",0),

		OPEN_BLOCK("{",100),
		CLOSE_BLOCK("}",0),

		EQ("==",50),
		NE("!=",50),
		LT("<",60),
		LE("<=",60),
		GT(">",60),
		GE(">=",60),

		AND("&&",30),
		OR("||",25),
		NOT("!",60);

		/**
		 * 文字列値
		 */
		private String val;
		/**
		 * 優先順位
		 */
		private int priority;
		
		/**
		 * コンストラクタ
		 * @param val 演算子の値
		 * @param priority 演算子の優先順位
		 */
		private Operator(String val,int priority) {
			this.val = val;
			this.priority = priority;
		}

		/**
		 * 対象文字列
		 */
		public String toString() {
			return this.name();
		}
	
		/**
		 * 優先順位の取得
		 * @return 優先順位
		 */
		public int getPriority() {
			return priority;
		}

		/**
		 * インデックスの取得
		 */
		@Override
		public int getLastIndex(String v) {
			int index = v.indexOf(this.val);
			if ( index == 0 ) {
				return this.val.length();
			}
			return -1;
		}

		/**
		 * 比較演算子
		 * <pre>
		 * EQ,NEはequals()で処理している為、ここには加えていない。
		 * ※StringもcompareTo==0で判定できるので加えてもいいかも
		 * </pre>
		 * @return 比較演算子の場合true
		 */
		boolean isComparable() {
			if ( this.equals(LT) || this.equals(LE) ||
			     this.equals(GT) || this.equals(GE) ) {
				return true;
			}
			return false;
		}

		/**
		 * 論理演算子
		 * @return 論理演算子の場合true
		 */
		boolean isLogical() {
			if ( this.equals(AND) || this.equals(OR) || this.equals(NOT) ) {
				return true;
			}
			return false;
		}

		/**
		 * 計算式
		 * @return 計算式の場合true
		 */
		boolean isCalc() {
			if ( this.equals(PLUS) || this.equals(MINUS) ||
			     this.equals(MUL) || this.equals(DIV) || this.equals(MOD) ) {
				return true;
			}
			return false;
		}
	}

	/**
	 * 値
	 * <pre>
	 * 正規表現で判定するタイプ
	 * </pre>
	 */
	public enum Value implements Type {

		/**
		 * 文字列
		 */
		STRING("\\\"[^\\\"]+\\\""),
		/**
		 * 整数
		 */
		INTEGER("\\d+"),
		/**
		 * 小数
		 */
		REAL ("\\d+\\.\\d+"),
		/**
		 * グローバル変数
		 */
		VARIABLE("\\$\\w+"),
		/**
		 * 関数呼び出し
		 */
		INVOKER("(\\w+)(\\.)(\\w+)"),
		/**
		 * その他
		 */
		IDENTIFIER("\\w+");

		/**
		 * 文字列パターン
		 */
		private Pattern pattern;
		/**
		 * コンストラクタ
		 * @param val パターン文字列
		 */
		private Value(String val) {
			pattern = Pattern.compile(val);
		}

		/**
		 * インデックス取得
		 * <pre>
		 * 正規表現のマッチする位置までを返す
		 * </pre>
		 */
		public int getLastIndex(String v) {
			Matcher m = pattern.matcher(v);
			if ( m.find() ) {
				if ( m.start() == 0 ) {
					return m.end();
				}
			}
			return -1;
		}
	}

}
