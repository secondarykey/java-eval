package com.github.secondarykey.calculator;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.LogManager;

public class TestUtil {
	static {
	    try {
			LogManager.getLogManager().readConfiguration(
			        Caluculator.class.getResourceAsStream("/logging.properties"));
		} catch (SecurityException | IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}	
	}

	public static String get(String name) {
		InputStream is = TestUtil.class.getResourceAsStream(name);
		if ( is == null ) {
			throw new RuntimeException("読み込みエラー:"+name);
		}
		BufferedInputStream bis = new BufferedInputStream(is);
		byte[] arrByte = new byte[1024];
		try {
			while ( (bis.read(arrByte, 0, arrByte.length) ) != -1) {}
		} catch (IOException e) {
			throw new RuntimeException("読み込みエラー",e);
		}
		return new String(arrByte);
	}
}
