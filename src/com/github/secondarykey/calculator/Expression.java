package com.github.secondarykey.calculator;

import java.util.ArrayList;
import java.util.List;

import com.github.secondarykey.calculator.Token.Control;
import com.github.secondarykey.calculator.Token.Operator;
import com.github.secondarykey.calculator.Token.Type;
import com.github.secondarykey.calculator.Token.Value;
import com.github.secondarykey.calculator.util.ClassUtil;

/**
 * 評価器
 * <pre>
 * １行の実装を元に
 * 字句解析(Lexer)、構文解析(Paser)、評価(evalute)を行う
 * </pre>
 */
public class Expression {

	private Token ast;

	/**
	 * コンストラクタ
	 * <pre>
	 * 字句解析、構文解析を行い、構文木を保持する
	 * </pre>
	 * @param line 計算式
	 */
	public Expression(String line) {
		//字句解析実行
		Lexer lex = new Lexer(line);
		List<Token> tokenList = lex.analysis();

		ast = parse(tokenList);
	}


	/**
	 * 構文解析
	 * <pre>
	 * 字句解析リストから構文解析を行う
	 * 現状では構文木が２本になる予定はない
	 * </pre>
	 */
	private Token parse(List<Token> values) {
		Parser parser = new Parser(values);
		List<Token> rtn = new ArrayList<>();
		while( parser.hasNext() ) {
			rtn.add(parser.get(0));
		}
		if ( rtn.size() > 1 ) {
			throw new AstException("現状想定してない構文木の可能性があります。");
		}
		return rtn.get(0);
	}

	/**
	 * 解析
	 * <pre>
	 * 構文木の解析を行う
	 * </pre>
	 * @param arguments プログラミング引数
	 */
	public Object eval(Variable arguments) {
		return expression(ast,arguments);
	}

	/**
	 * 解析
	 * <pre>
	 * トークンに対して実行を行う
	 * </pre>
	 * @param token 解析対象トークン
	 * @param args プログラミング引数
	 */
	private Object expression(Token token, Variable args) {

		Type type = token.getType();
		String val = token.getValue();

		if ( type == Operator.NOT ) {
			Object right = expression(token.right(),args);	
			if ( !ClassUtil.isBoolean(right) ) {
				throw new ExpressionException("Not演算子に対してBooelanじゃない値が入っています");
			}
			return !(Boolean)right;
		}

		if ( type instanceof Value ) {
			
			if ( type == Value.STRING ) {
				return val;
			} else if ( type == Value.INTEGER ) {
				return Integer.parseInt(val);
			} else if ( type == Value.REAL ) {
				return Double.parseDouble(val);
			} else if ( type == Value.IDENTIFIER ) {
				if ( val.equals("null") ) {
					return null;
				} else if ( val.equals("true") ) {
					return true;
				} else if ( val.equals("false") ) {
					return false;
				}
				throw new ExpressionException("予約語が存在しません[" + val + "]");
			} else if ( type == Value.VARIABLE ) {
				return args.get(val);
			} else if ( type == Value.INVOKER ) {

				int dot = val.indexOf(".");
				String valName = val.substring(0, dot);
				String funcName = val.substring(dot+1);
			
				Object valObj = args.get(valName);
	
				Object[] methodArgs = null;

				Token right = token.right();
				if ( right.getType() != Control.NOPARAM ) {
					//TODO 現状１つしかオブジェクトを返せない
					Object arg = expression(right,args);	
					methodArgs = new Object[1];
					methodArgs[0] = arg;
				}
				
				return ClassUtil.call(valObj,funcName,methodArgs);
			}
		} else if ( type instanceof Operator ) {
			
			Object left  = expression(token.left(),args);
			Object right = expression(token.right(),args);

			Class<? extends Object> clazz = left.getClass();
			if ( clazz != right.getClass() ) {
				throw new RuntimeException("" + clazz.getSimpleName() + " != " + right.getClass().getSimpleName());
			}

			//評価を行う
			return operation((Operator)type,left,right);
		}

		throw new ExpressionException("判定がないタイプです" + type.name());
	}


	/**
	 * 演算子処理を行う
	 * @param type
	 * @param left
	 * @param right
	 * @return
	 */
	private Object operation(Operator op, Object left, Object right) {
	
		//TODO 同じクラスかを判定
		if ( op == Operator.EQ ) {
			return left.equals(right);
		} else if ( op == Operator.NE ) {
			return !left.equals(right);
		} else if ( op.isComparable() ) {
			//右辺、左辺のチェック
			if ( ClassUtil.isComparableNumber(left) && 
					ClassUtil.isComparableNumber(right) ) {
				@SuppressWarnings("unchecked")
				boolean rtn = compareToNumber(op,(Comparable<Number>)left,(Number)right);
				return rtn;
			}
			throw new ExpressionException("比較演算子の型がサポートされていません");
		} else if ( op.isLogical() ) {
			if ( ClassUtil.isBoolean(left) && 
					ClassUtil.isBoolean(right) ) {
				return logical(op,(Boolean)left,(Boolean)right);
			}
			throw new ExpressionException("論理演算子の型がサポートされていません");
		}
		throw new ExpressionException("演算子がサポートされていません:" + op);
	}

	/**
	 * 論理演算子の実行
	 * @param op
	 * @param left
	 * @param right
	 * @return
	 */
	private Object logical(Operator op, Boolean left, Boolean right) {
		if ( Operator.AND.equals(op) ) {
			return left && right;
		} else if ( Operator.OR.equals(op) ) {
			return left || right;
		} else if ( Operator.NOT.equals(op) ) {
			// NOT も入るけど、呼び出し関数側ですでにチェックされている
			return !right;
		}
		throw new ExpressionException("想定してない判定:" + op);
	}


	private boolean compareToNumber(Operator op,Comparable<Number> left,Number right) {
		int rtn = left.compareTo(right);
		if ( Operator.LT.equals(op) ) {
			return rtn < 0;
		} else if ( Operator.LE.equals(op) ) {
			return rtn <= 0;
		} else if ( Operator.GT.equals(op) ) {
			return rtn > 0;
		} else if ( Operator.GE.equals(op) ) {
			return rtn >= 0;
		}
		throw new ExpressionException("想定してない判定:" + op);
	}

	/**
	 * 評価器の例外
	 */
	private class ExpressionException extends RuntimeException {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public ExpressionException(String string) {
			super(string);
		}
	}

	/**
	 * 構文木例外 
	 */
	private class AstException extends RuntimeException {
		private static final long serialVersionUID = 1L;
		public AstException(String string) {
			super(string);
		}
	}
}
