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
		
		System.out.println("Class:" + target.getClass().getSimpleName());
		System.out.println("Method:" + name);

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
				System.out.println(idx + ":" + arg.getClass().getSimpleName());
				idx++;
			}
		} else {
			System.out.println("引数なし");
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
				
				System.out.println(idx + ":" + type.getSimpleName());
				System.out.println(idx + ":" + argsClazz[idx].getSimpleName());

				//型が派生関係にあるかを確認
				if ( !same(type,argsClazz[idx]) ) {
					
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

	private static boolean same(Class<?> clazz1, Class<?> clazz2) {
		if( clazz1.equals(clazz2) ) {
			return true;
		} else if( isWrap(clazz1,clazz2) ) {
			return true;
		} else if( clazz1.isAssignableFrom(clazz2) ) {
			return true;
		}
		return false;
	}

	/**
	 * プリミティブを比較
	 * @param clazz1
	 * @param clazz2
	 * @return
	 */
	private static boolean isWrap(Class<?> clazz1, Class<?> clazz2) {
		if ( isInteger(clazz1) ) {
			if ( isInteger(clazz2) ) return true;
			return false;
		} else if ( isDouble(clazz1) ) {
			if ( isDouble(clazz2) ) return true;
			return false;
		} else if ( isByte(clazz1) ) {
			if ( isByte(clazz2) ) return true;
			return false;
		} else if ( isLong(clazz1) ) {
			if ( isLong(clazz2) ) return true;
			return false;
		} else if ( isShort(clazz1) ) {
			if ( isShort(clazz2) ) return true;
			return false;
		} else if ( isFloat(clazz1) ) {
			if ( isFloat(clazz2) ) return true;
			return false;
		} else if ( isChar(clazz1) ) {
			if ( isChar(clazz2) ) return true;
			return false;
		}
		return false;
	}
	
	public static Boolean isInteger(Class<?> clazz) {
		if ( clazz == int.class ) {
			return true;
		} else if ( clazz == Integer.class ) {
			return true;
		}
		return false;
	}
	public static Boolean isDouble(Class<?> clazz) {
		if ( clazz == double.class ) {
			return true;
		} else if ( clazz == Double.class ) {
			return true;
		}
		return false;
	}
	public static Boolean isByte(Class<?> clazz) {
		if ( clazz == byte.class ) {
			return true;
		} else if ( clazz == Byte.class ) {
			return true;
		}
		return false;
	}
	public static Boolean isShort(Class<?> clazz) {
		if ( clazz == short.class ) {
			return true;
		} else if ( clazz == Short.class ) {
			return true;
		}
		return false;
	}
	public static Boolean isLong(Class<?> clazz) {
		if ( clazz == long.class ) {
			return true;
		} else if ( clazz == Long.class ) {
			return true;
		}
		return false;
	}
	public static Boolean isFloat(Class<?> clazz) {
		if ( clazz == float.class ) {
			return true;
		} else if ( clazz == Float.class ) {
			return true;
		}
		return false;
	}
	public static Boolean isChar(Class<?> clazz) {
		if ( clazz == char.class ) {
			return true;
		} else if ( clazz == Character.class ) {
			return true;
		}
		return false;
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
