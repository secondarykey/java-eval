package com.github.secondarykey.calculator;

import java.util.ArrayList;
import java.util.List;

/**
 * �v�Z�@
 * <pre>
 * �ϐ��̒l�ƕ]�����ێ����A���s���s��
 * �A�����s���A�^�����C���f�b�N�X��Ԃ�
 * </pre>
 * @author secon
 */
public class Calculator {

	private List<Expression> expList = new ArrayList<>();
	private Variable param = new Variable();
	public void addVariable(String name,Object value) {
		param.add(name, value);
	}
	
	public int eval() {
		for ( Expression exp : expList ) {
		}
		return -1;
	}

}
