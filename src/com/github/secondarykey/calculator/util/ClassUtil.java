package com.github.secondarykey.calculator.util;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * リフレクション用のユーティリティ
 * @author secon
 */
public class ClassUtil {

	/**
	 * メソッド呼び出し
	 * <pre>
	 * 引数を派生関係を見て判定する
	 * </pre>
	 * @param target 対象オブジェクト
	 * @param name メソッド名称
	 * @param args 引数
	 * @return 戻り値
	 */
	public static Object call(Object target,String name,Object... args) {

		Class<?> clazz = target.getClass();
		Class<?>[] argsClazz = null;

		//引数指定がある場合引数のクラス群を設定
		if ( args != null ) {
			argsClazz = new Class<?>[args.length];
			int idx = 0;
			//引数型の作成
			for ( Object arg : args ) {
				if ( arg == null ) {
					throw new RuntimeException("Nullを許容していません");
				}
				argsClazz[idx] = arg.getClass();
				idx++;
			}
		}

		Method method = null;
		Method[] ms = clazz.getMethods();
		for ( Method m : ms ) {
			//名称が同一じゃないと処理しない
			if ( !m.getName().equals(name) ) {
				continue;
			}
			Class<?>[] types = m.getParameterTypes();
			if ( argsClazz == null ) {
				if ( types != null && types.length != 0 ) {
					continue;
				}
				method = m;
				break;
			} else {
				if ( types.length != argsClazz.length ) {
					continue;
				}
			}
		
			boolean ok = true;
			int idx = 0;


			for ( Class<?> type : types ) {
				//型が派生関係にあるかを確認
				if ( !type.isAssignableFrom(argsClazz[idx]) ) {
					ok = false;
					break;
				}
				idx++;
			}
			

			if ( ok ) {
				method = m;
				break;
			}
		}
		

		try {
			if ( method == null ) {
				throw new NoSuchMethodException("メソッドの検索に失敗しています");
			}
			if ( args == null ) {
				return method.invoke(target);
			}
			return method.invoke(target,args);
		} catch (NoSuchMethodException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
			throw new RuntimeException("Method呼び出し時の例外",e);
		}
	}

	/**
	 * Comparable,Numberを実装しているかを判定
	 * @param obj 対象オブジェクト
	 * @return 型があっている場合true
	 */
	public static boolean isComparableNumber(Object obj) {
		if ( obj instanceof Comparable ) {
			if ( obj instanceof Number ) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Booleanチェック
	 * @param obj
	 * @return
	 */
	public static boolean isBoolean(Object obj) {
		if ( obj instanceof Boolean ) {
			return true;
		}
		return false;
	}
}
