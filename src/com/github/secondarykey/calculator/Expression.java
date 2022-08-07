package com.github.secondarykey.calculator;

import java.util.List;
import java.util.logging.Logger;

import com.github.secondarykey.calculator.Token.Operator;
import com.github.secondarykey.calculator.Token.Type;
import com.github.secondarykey.calculator.Token.Value;
import com.github.secondarykey.calculator.util.ClassUtil;

/**
 * 評価器
 * <pre>
 * 実装を元に字句解析(Lexer)、構文解析(Paser)、評価(evalute)を行う
 * </pre>
 */
public class Expression {

	@SuppressWarnings("unused")
	private static final Logger logger = Logger.getLogger(Expression.class.getName());

	/**
	 * 字句解析器(デバッグ用)
	 */
	private Lexer lex;

	/**
	 * 構文木
	 */
	private Ast ast;

	/**
	 * コンストラクタ
	 * <pre>
	 * 字句解析、構文解析を行い、構文木を保持する
	 * </pre>
	 * @param line 計算式
	 */
	public Expression(String line) {
		//字句解析実行
		lex = new Lexer(line);
		ast = AstParser.parse(lex);
	}

	/**
	 * 解析
	 * <pre>
	 * 構文木の解析を行う
	 * </pre>
	 * @param arguments プログラミング引数
	 */
	public Object eval(Variable arguments) {

		List<Token> tokens = ast.get();
		for ( Token token : tokens ) {
			Object rtn;
			try {
				rtn = expression(token,arguments);
			} catch (Return e) {
				//Return時はその値を返す
				return e.get();
			}

			//TODO どう扱うかを処理する
			if ( tokens.size() == 1 ) {
				return rtn;
			}
		}
		return null;
	}

	/**
	 * 解析
	 * <pre>
	 * トークンに対して実行を行う
	 * </pre>
	 * @param token 解析対象トークン
	 * @param args プログラミング引数
	 */
	private Object expression(Token token, Variable var) throws Return {

		Type type = token.getType();
		String val = token.getValue();

		if ( type == Operator.NOT ) {
			Token right = token.right();	
			Object rightAns = expression(right,var);	
			if ( !ClassUtil.isBoolean(rightAns) ) {
				throw new ExpressionException(lex,right,"Not演算子に対してBooelanじゃない値が入っています");
			}
			return !(Boolean)rightAns;
		}

		if ( type instanceof Value ) {
			
			if ( type.equals(Value.STRING) ) {
				return val;
			} else if ( type.equals(Value.INTEGER) ) {
				return Integer.parseInt(val);
			} else if ( type.equals(Value.REAL) ) {
				return Double.parseDouble(val);
			} else if ( type.equals(Value.IDENTIFIER) ) {

				if ( val.equals("null") ) {
					return null;
				} else if ( val.equals("true") ) {
					return true;
				} else if ( val.equals("false") ) {
					return false;
				} else if ( val.equals("return") ) {
					Token right = token.right();
					throw new Return(expression(right,var)); 
				} else if ( val.equals("if") ) {
					
					Token right = token.right();
					Object rtn = expression(right,var);
					if ( !ClassUtil.isBoolean(rtn) ) {
						throw new ExpressionException(lex,right,"if文の右辺がBoolean型ではありません。[" + rtn.getClass().getSimpleName() + "]");
					} else {
						Boolean e = (Boolean)rtn;
						if ( e ) {
							List<Token> blocks = token.getBlocks();
							for ( Token child : blocks ) {
								expression(child,var);
							}
						}
						return e;
					}
				} else if ( val.equals("let") ) {
					Token name = token.right();
					Token v = name.right();
					//宣言の追加
					var.addLocal(name.getValue(),expression(v,var));
					return null;
				}

				//TODO もう少し考える
				Object obj = var.getLocal(val);
				if ( obj != null ) {
					return obj;
				}
	
				throw new ExpressionException(lex,token,"予約語、変数が存在しません[" + val + "]");

			} else if ( type.equals(Value.VARIABLE) ) {
				return var.get(val);
			} else if ( type.equals(Value.INVOKER) ) {
				
				//TODO 複数引数の対応

				int dot = val.indexOf(".");
				String valName = val.substring(0, dot);
				String funcName = val.substring(dot+1);

				Object valObj = var.get(valName);
				if ( valObj == null ) {
					//TODO Lexerで別の値にローカルかを判定しておく
					valObj = var.getLocal(valName);
				}

				Object[] methodArgs = null;

				//TODO right ではなくブロックがいいかな、、、
				List<Token> args = token.getBlocks();
				methodArgs = new Object[args.size()];
				int idx = 0;
				for ( Token arg : args ) {
					Object ans = expression(arg,var);	
					methodArgs[idx] = ans;
					idx++;
				}
				return ClassUtil.call(valObj,funcName,methodArgs);
			}
		} else if ( type instanceof Operator ) {

			Object left  = expression(token.left(),var);
			Object right = expression(token.right(),var);

			Class<? extends Object> clazz = left.getClass();
			if ( clazz != right.getClass() ) {
				//右辺と左辺の型が違う
				throw new RuntimeException("" + clazz.getSimpleName() + " != " + right.getClass().getSimpleName());
			}

			//評価を行う
			return operation(token,(Operator)type,left,right);
		}

		throw new ExpressionException(lex,token,"判定がないタイプです" + type);
	}


	/**
	 * 演算子処理を行う
	 * @param type
	 * @param left
	 * @param right
	 * @return
	 */
	private Object operation(Token token,Operator op, Object left, Object right) {
	
		if ( op == Operator.EQ ) {
			return left.equals(right);
		} else if ( op == Operator.NE ) {
			return !left.equals(right);
		} else if ( op.isComparable() ) {
			//右辺、左辺のチェック
			if ( ClassUtil.isComparableNumber(left) && 
					ClassUtil.isComparableNumber(right) ) {
				@SuppressWarnings("unchecked")
				boolean rtn = compareToNumber(token,op,(Comparable<Number>)left,(Number)right);
				return rtn;
			}
			throw new ExpressionException(lex,token,"比較演算子の型がサポートされていません:[" + left.getClass().getSimpleName() + "]");
		} else if ( op.isCalc() ) {
			return calc(token,op,left,right);
		} else if ( op.isLogical() ) {
			if ( ClassUtil.isBoolean(left) && 
					ClassUtil.isBoolean(right) ) {
				return logical(token,op,(Boolean)left,(Boolean)right);
			}
			throw new ExpressionException(lex,token,"論理演算子の型がサポートされていません:[" + left.getClass().getSimpleName() + "]");
		}
		throw new ExpressionException(lex,token,"演算子がサポートされていません:" + op);
	}

	/**
	 * 計算
	 * @param op
	 * @param left
	 * @param right
	 * @return
	 */
	private Object calc(Token token,Operator op, Object left, Object right) {

		boolean d = true;
		if ( left instanceof Integer ) {
			d = false;
		} else if ( !(left instanceof Double) ) {
			if ( (left instanceof String) && op.equals(Operator.PLUS) ) {
				return (String)left + (String)right;
			}
			throw new ExpressionException(lex,token,String.format("%s は計算(%s)をサーポートしていません。",left.getClass().getSimpleName(),op));
		}

		if ( op.equals(Operator.PLUS) ) {
			if ( d ) return (Double)left + (Double)right;
			return (Integer)left + (Integer)right;
		} else if ( op.equals(Operator.MINUS) ) {
			if ( d ) return (Double)left - (Double)right;
			return (Integer)left - (Integer)right;
		} else if ( op.equals(Operator.MUL) ) {
			if ( d ) return (Double)left * (Double)right;
			return (Integer)left * (Integer)right;
		} else if ( op.equals(Operator.DIV) ) {
			if ( d ) return (Double)left / (Double)right;
			return (Integer)left / (Integer)right;
		} else if ( op.equals(Operator.MOD) ) {
			if ( d ) return (Double)left % (Double)right;
			return (Integer)left % (Integer)right;
		}

		throw new ExpressionException(lex,token,String.format("計算をサポートしてない演算子(%s)です。",op));
	}

	/**
	 * 論理演算子の実行
	 * @param op
	 * @param left
	 * @param right
	 * @return
	 */
	private Object logical(Token token,Operator op, Boolean left, Boolean right) {
		if ( Operator.AND.equals(op) ) {
			return left && right;
		} else if ( Operator.OR.equals(op) ) {
			return left || right;
		} else if ( Operator.NOT.equals(op) ) {
			// NOT も入るけど、呼び出し関数側ですでにチェックされている
			return !right;
		}
		throw new ExpressionException(lex,token,"想定してない判定:" + op);
	}


	/**
	 * 数値比較
	 * @param token 対象トークン（デバッグ用）
	 * @param op オペレーター
	 * @param left 左辺の値
	 * @param right 右辺の値
	 * @return 対象値
	 */
	private boolean compareToNumber(Token token,Operator op,Comparable<Number> left,Number right) {
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
		throw new ExpressionException(lex,token,"想定してない判定:" + op);
	}

	/**
	 * 評価器の例外
	 * <pre>
	 * 字句解析器を元にエラー位置を特定し、文字列として返す
	 * </pre>
	 */
	private class ExpressionException extends RuntimeException {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		/**
		 * 対象字句解析器
		 */
		private Lexer lex;
		/**
		 * 対象トークン
		 */
		private Token token;
		/**
		 * コンストラクタ
		 * @param lex 対象字句解析器
		 * @param token 対象トークン
		 * @param string 対象メッセージ
		 */
		public ExpressionException(Lexer lex,Token token,String string) {
			super(string);
			this.lex = lex;
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

	/**
	 * Return用例外
	 * <pre>
	 * return 処理時に発生
	 * 再帰等関係なく、値をそのまま返す為に利用
	 * </pre>
	 * @author secon
	 */
	private class Return extends Exception {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		/**
		 * return対象
		 */
		private Object obj;
		
		/**
		 * コンストラクタ
		 * @param obj 戻り値
		 */
		public Return(Object obj) {
			this.obj = obj;
		}
		
		/**
		 * return 対象の取得
		 * @return 戻り値
		 */
		public Object get() {
			return obj;
		}
	}

}
