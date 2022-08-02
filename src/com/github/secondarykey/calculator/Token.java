package com.github.secondarykey.calculator;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 */
public class Token {

	private Type type;
	private String value;
	
	private Token left;
	private Token right;

	public Token(Type type, String val) {
		this.type = type;
		this.value = val;
	}

	public Type getType() {
		return type;
	}
	
	public String getValue() {
		return value;
	}
	
	public Token left() {
		return left;
	}
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
	 * @return
	 */
	public int getPriority() {
		if ( type instanceof Operator ) {
			return ((Operator)type).getPriority();
		} else if ( type.equals(Control.EOT) ) {
			return -1;
		}
		throw new RuntimeException("優先順位は存在しないはず" + this);
	}

	public String toString() {
		return String.format("%-15s->%s",type,value);
	}

	/**
	 */
	public interface Type {
		int getLastIndex(String val);
		String name();
	}

	/**
	 * 制御系のデータ
	 * @author secon
	 */
	public enum Control implements Type {

		NOPARAM,
		EOT;

		@Override
		public int getLastIndex(String val) {
			return -1;
		}

	}
	/**
	 * 演算子
	 * @author secon
	 */
	public enum Operator implements Type {

		//PLUS("+",70),
		//MINUS("-",70),
		//MUL("*",80),
		//DIV("/",80),
		//MOD("%",80),

		OPEN("(",100),
		CLOSE(")",0),

		IFOPEN("{",100),
		IFCLOSE("}",0),

		EQ("==",50),
		NE("!=",50),
		LT("<",60),
		LE("<=",60),
		GT(">",60),
		GE(">=",60),

		AND("&&",30),
		OR("||",25),
		NOT("!",60);

		private String val;
		private int priority;
		private Operator(String val,int priority) {
			this.val = val;
			this.priority = priority;
		}

		public String toString() {
			return this.name();
		}
		
		public int getPriority() {
			return priority;
		}

		public int getLastIndex(String v) {
			int index = v.indexOf(this.val);
			if ( index == 0 ) {
				return index + this.val.length();
			}
			return -1;
		}

		/**
		 * 
		 * @return
		 */
		boolean isComparable() {
			if ( this.equals(LT) || this.equals(LE) ||
			     this.equals(GT) || this.equals(GE) ) {
				return true;
			}
			return false;
		}

		boolean isLogical() {
			if ( this.equals(AND) || this.equals(OR) || this.equals(NOT) ) {
				return true;
			}
			return false;
		}
	}

	/**
	 * 値
	 * <pre>
	 * </pre>
	 */
	public enum Value implements Type {

		STRING("\\\"[^\\\"]+\\\""),
		INTEGER("\\d+"),
		REAL ("\\d+\\.\\d+"),
		VARIABLE("\\$\\w+"),
		INVOKER("(\\$\\w+)(\\.)(\\w+)"),
		IDENTIFIER("\\w+");

		private Pattern pattern;
		private Value(String val) {
			pattern = Pattern.compile(val);
		}
	
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
