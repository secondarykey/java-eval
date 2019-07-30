package com.github.secondarykey.calculator;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import com.github.secondarykey.calculator.Token.Control;
import com.github.secondarykey.calculator.Token.Operator;
import com.github.secondarykey.calculator.Token.Type;
import com.github.secondarykey.calculator.Token.Value;

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
			if ( !(right instanceof Boolean) ) {
				throw new RuntimeException("Not演算子に対してBooelanじゃない値が入っています");
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
				throw new RuntimeException("予約語が存在しません[" + val + "]");
				 
			} else if ( type == Value.VARIABLE ) {
				return args.get(val);
			} else if ( type == Value.INVOKER ) {

				int dot = val.indexOf(".");
				String valName = val.substring(0, dot);
				String funcName = val.substring(dot+1);
			
				Object valObj = args.get(valName);
				Class<?> clazz = valObj.getClass();
				
				Token right = token.right();
				if ( right.getType() == Control.NOPARAM ) {
					Method method;
					try {
						method = clazz.getMethod(funcName);
						return method.invoke(valObj);
					} catch (NoSuchMethodException | SecurityException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
						throw new ExpressionException("関数実行例外(引数なし):" + funcName,e);
					}
				} else {

					Object arg = expression(right,args);	
					String className = "";
					try {
						Class<? extends Object> argClass = arg.getClass();
						className = argClass.getSimpleName();
						Method[] methods = clazz.getMethods();
						Method method = null;
						for ( Method wk : methods ) {
							if ( !wk.getName().equals(funcName) ) {
								continue;
							}
							Class<?>[] types = wk.getParameterTypes();
							if ( types.length != 1 ) {
								continue;
							}

							if ( types[0].isAssignableFrom(argClass) ) {
								System.out.println(types[0].getName());
								method = wk;
							}
						}

						if ( method == null ) {
							throw new NoSuchMethodException();
						}
						
						return method.invoke(valObj,arg);
					} catch (NoSuchMethodException | SecurityException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
						throw new ExpressionException("関数実行例外:" + funcName + ",引数型:" + className,e);
					}
				}
			}
		} else if ( type instanceof Operator ) {
			
			Object left  = expression(token.left(),args);
			Object right = expression(token.right(),args);

			Class<? extends Object> clazz = left.getClass();
			if ( clazz != right.getClass() ) {
				throw new RuntimeException("" + clazz.getSimpleName() + " != " + right.getClass().getSimpleName());
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

		throw new ExpressionException("判定がないタイプです" + type.name());
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

		public ExpressionException(String string, Exception e) {
			super(string,e);
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
