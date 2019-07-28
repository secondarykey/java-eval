package com.github.secondarykey.calculator;

import java.util.ArrayList;
import java.util.List;

import com.github.secondarykey.calculator.Token.Operator;
import com.github.secondarykey.calculator.Token.Type;
import com.github.secondarykey.calculator.Token.Value;

/**
 * ����p�̕]����
 * <pre>
 * �ϐ��𖄂ߍ���ŁA���̕������true/false�𔻒�ł���]����
 * </pre>
 * @author secon
 */
public class Expression {

	/** ��͂����\���؁@**/
	private List<Token> ast;

	/**
	 * �R���X�g���N�^
	 * <pre>
	 * �����͂ƍ\����͂��s��
	 * </pre>
	 * @param line
	 */
	public Expression(String line) {
		List<Token> tokenList = lexical(line);
		ast = parse(tokenList);
	}

	/**
	 * �����͂��s��
	 * @param line
	 * @return
	 */
	private List<Token> lexical(String line) {
		Lexer lex = new Lexer(line);
		return lex.analysis();
	}

	/**
	 * Token�̗D�揇�ʂ����āA�\����ݒ肷��
	 * @param values �����͌�̃g�[�N��
	 * @return �\����ݒ肵���g�[�N��
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
	 * ���r�����s
	 * @param arguments ����
	 * @return 
	 */
	public Object eval(Variable arguments) {
		if ( ast.size() > 1 ) {
			System.out.println("�\���؂���{");
		}

		for ( Token token : ast ) {
			Object obj = expression(token,arguments);
			return obj;
		}
		throw new RuntimeException("�\���؂����݂��Ȃ�");
	}

	/**
	 * Token���̌v�Z
	 * @param token �Ώۂ̃g�[�N��
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
				throw new RuntimeException("����null�ȊO�̕�����l�̓T�|�[�g���Ă��܂���B");
			} else if ( type == Value.VARIABLE ) {
				return args.get(val);
			}
		} else if ( type instanceof Operator ) {
			
			Object left  = expression(token.left(),args);
			Object right = expression(token.right(),args);

			Class<? extends Object> clazz = left.getClass();
			if ( clazz != right.getClass() ) {
				throw new RuntimeException("��r���Ă���N���X���Ⴂ�܂�" + clazz.getSimpleName() + " != " + right.getClass().getSimpleName());
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

		throw new RuntimeException("�z�肵�ĂȂ��g�[�N���^�C�v�ł��B" + type.name());
	}
}
