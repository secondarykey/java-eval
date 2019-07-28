package com.github.secondarykey.calculator;

import java.util.ArrayList;
import java.util.List;

import com.github.secondarykey.calculator.Token.Operator;
import com.github.secondarykey.calculator.Token.Type;
import com.github.secondarykey.calculator.Token.Value;

/**
 * �����͊�
 * @author secon
 */
public class Lexer {

	private String value;

	private static List<Type> types = null;

	private static void init() {
		//��`�̓ǂݍ���
		if ( types == null ) {
			types = new ArrayList<Type>();
			for ( Operator op : Operator.values() ) {
				types.add(op);
			}
			for ( Value val : Value.values() ) {
				types.add(val);
			}
		}
		return;
	}

	public Lexer(String line) {
		init();
		value = line;
	}
	
	/**
	 * ������
	 * <pre>
	 * ���Z�q���̉�ɕύX����
	 * </pre>
	 * @return �g�[�N���̃��X�g
	 */
	public List<Token> analysis() {

		List<Token> rtn = new ArrayList<Token>();
		String buf = skip(value);

		while ( !buf.isEmpty() ) {
			boolean notfound = true;
			for (Type t : types) {
            	int index = t.getLastIndex(buf);
            	if ( index != -1 ) {
            		int pre = 0;
            		int suf = index;
            		if ( t.equals(Value.VARIABLE) ) {
            			//$���폜
            			pre = 1;
            		} else if ( t.equals(Value.STRING) ) {
            			//DQ���폜
            			pre = 1;
            			suf = index - 1;
            		} else if ( t.equals(Operator.GT) ) {
            			int wk = Operator.GE.getLastIndex(buf);
            			if ( wk != -1 ) {
            				index = wk;
            				suf = index;
            				t = Operator.GE;
            			}
            		} else if ( t.equals(Operator.LT) ) {
            			int wk = Operator.LE.getLastIndex(buf);
            			if ( wk != -1 ) {
            				index = wk;
            				suf = index;
            				t = Operator.LE;
            			}
            		} else if ( t.equals(Operator.NOT) ) {
            			int wk = Operator.NE.getLastIndex(buf);
            			if ( wk != -1 ) {
            				index = wk;
            				suf = index;
            				t = Operator.NE;
            			}
            		}

            		String val = buf.substring(pre, suf);
            		Token token = new Token(t,val);
            		rtn.add(token);

            		buf = buf.substring(index);
            		notfound = false;
            		break;
            	}
        	}
			
			buf = skip(buf);
			if ( notfound ) {
				throw new LexerException("�����͎��ɉ�͕s�\�Ȏ��傪���݂��܂����B[" + buf  + "]");
			}
		}
		
		//�I�[�����ĕԂ�
        return rtn;
	}	
	

	/**
	 * �����̓X�L�b�v
	 * @param buf
	 * @return
	 */
	private String skip(String buf) {
		//����A�󔒂���
		return buf.trim();
	}	
	
	/**
	 * �����̗͂�O
	 * @author secon
	 */
	public class LexerException extends RuntimeException {
		private static final long serialVersionUID = 1L;
		public LexerException(String string) {
			super(string);
		}
	}
}
