package com.github.secondarykey.calculator;

import java.util.ArrayList;
import java.util.List;

import com.github.secondarykey.calculator.Token.Operator;
import com.github.secondarykey.calculator.Token.Type;
import com.github.secondarykey.calculator.Token.Value;

/**
 * 判定用の評価器
 * <pre>
 * 変数を埋め込んで、その文字列のtrue/falseを判定できる評価器
 * </pre>
 * @author secon
 */
public class Expression {

	/** 解析した構文木　**/
	private List<Token> ast;

	/**
	 * コンストラクタ
	 * <pre>
	 * 字句解析と構造解析を行う
	 * </pre>
	 * @param line
	 */
	public Expression(String line) {
		List<Token> tokenList = lexical(line);
		ast = parse(tokenList);
	}

	/**
	 * 字句解析を行う
	 * @param line
	 * @return
	 */
	private List<Token> lexical(String line) {
		Lexer lex = new Lexer(line);
		return lex.analysis();
	}

	/**
	 * Tokenの優先順位を見て、構造を設定する
	 * @param values 字句解析後のトークン
	 * @return 構造を設定したトークン
	 */
	private List<Token> parse(List<Token> values) {
		Parser parser = new Parser(values);
		List<Token> rtn = new ArrayList<>();
		while( parser.hasNext() ) {
			rtn.add(parser.get(0));
		}
		return rtn;
	}

	/**
	 * 研鑽を実行
	 * @param arguments 引数
	 * @return 
	 */
	public Object eval(Variable arguments) {
		if ( ast.size() > 1 ) {
			System.out.println("構文木が二本");
		}

		for ( Token token : ast ) {
			Object obj = expression(token,arguments);
			return obj;
		}
		throw new RuntimeException("構文木が存在しない");
	}

	/**
	 * Token内の計算
	 * @param token 対象のトークン
	 * @param arguments 
	 */
	private Object expression(Token token, Variable args) {
		Type type = token.getType();
		String val = token.getValue();

		if ( type instanceof Value ) {
			if ( type == Value.STRING ) {
				return val;
			} else if ( type == Value.INTEGER ) {
				return Integer.parseInt(val);
			} else if ( type == Value.REAL ) {
				return Double.parseDouble(val);
			} else if ( type == Value.IDENTIFIER ) {
				if ( val == "null" ) {
					return null;
				}
				throw new RuntimeException("現在null以外の文字列値はサポートしていません。");
			} else if ( type == Value.VARIABLE ) {
				return args.get(val);
			}
		} else if ( type instanceof Operator ) {
			
			Object left  = expression(token.left(),args);
			Object right = expression(token.right(),args);

			Class<? extends Object> clazz = left.getClass();
			if ( clazz != right.getClass() ) {
				throw new RuntimeException("比較しているクラスが違います" + clazz.getSimpleName() + " != " + right.getClass().getSimpleName());
			}

			if ( type == Operator.EQ ) {
				return left.equals(right);
			} else if ( type == Operator.NE ) {
				return !left.equals(right);
			} else if ( type == Operator.GT ) {
				if ( clazz == Integer.class ) {
					return (Integer)left > (Integer)right;
				} else {
					return (Double)left > (Double)right;
				}
			} else if ( type == Operator.GE ) {
				if ( clazz == Integer.class ) {
					return (Integer)left >= (Integer)right;
				} else {
					return (Double)left >= (Double)right;
				}
			} else if ( type == Operator.LT ) {
				if ( clazz == Integer.class ) {
					return (Integer)left < (Integer)right;
				} else {
					return (Double)left < (Double)right;
				}
			} else if ( type == Operator.LE ) {
				if ( clazz == Integer.class ) {
					return (Integer)left <= (Integer)right;
				} else {
					return (Double)left <= (Double)right;
				}
			} else if ( type == Operator.AND ) {
				if ( clazz == Boolean.class ) {
					return (Boolean)left && (Boolean)right;
				} else {
				}
			} else if ( type == Operator.OR ) {
				if ( clazz == Boolean.class ) {
					return (Boolean)left || (Boolean)right;
				}
			}
		}

		throw new RuntimeException("想定してないトークンタイプです。" + type.name());
	}
}
