package com.github.secondarykey.calculator;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import com.github.secondarykey.calculator.Token.Control;
import com.github.secondarykey.calculator.Token.Operator;
import com.github.secondarykey.calculator.Token.Type;
import com.github.secondarykey.calculator.Token.Value;

/**
 * 字句解析木
 * @author secon
 */
public class Lexer {

	@SuppressWarnings("unused")
	private static final Logger logger = Logger.getLogger(Lexer.class.getName());

	/**
	 * 判定用のタイプリスト
	 */
	private static List<Type> types = null;
	static {
		types = new ArrayList<Type>();
		for ( Control val : Control.values() ) {
			types.add(val);
		}
		for ( Operator op : Operator.values() ) {
			types.add(op);
		}
		for ( Value val : Value.values() ) {
			types.add(val);
		}
	}

	/**
	 * 解析元データ
	 */
	private String value;

	/**
	 * デバッグ用のポジション
	 */
	private int position;
	/**
	 * デバッグ用行数(改行する際のインデックス値)
	 */
	private int lineIndex = 1;

	/**
	 * 戻り値(字句解析後のリスト)
	 */
	private List<Token> tokenList;

	/**
	 * コンストラクタ
	 * @param line 対象データ
	 */
	public Lexer(String line) {

		logger.info("Lexer Start ===================================");
		logger.finest(line);
		value = line;
		//改行がWindowsの場合、行判定時のインデックスを２にする
		if ( value.indexOf("\r\n") != -1 ) {
			lineIndex = 2;
		}

		//トークンの解析を行う
		tokenList = tokenize();
		logger.info("Lexer End   ===================================");
	}

	/**
	 * 解析
	 * @return トークンリスト
	 */
	private List<Token> tokenize() {

		List<Token> rtn = new ArrayList<Token>();
		//初期のみ後方を削除する可能性がある為
		//skip()を呼び出すのではなく、trim()で行いデバッグ用のポジションを設定
		String buf = value.trim();
		if ( buf.length() != value.length() ) {
			//先頭位置をポジションに設定
			this.position = value.indexOf(buf);
		}

		//空文字で終了
		while ( !buf.isEmpty() ) {

			boolean notfound = true;

			for (Type t : types) {

            	int index = t.getLastIndex(buf);

           		//TODO 単項演算子(+,-)

            	//存在した場合
            	if ( index != -1 ) {

            		int pre = 0;
            		int suf = index;

            		if ( t.equals(Value.VARIABLE) ) {

            			//$削除用の位置
            			pre = 1;
            			//関数判定は$抜き
            			int wk = Value.INVOKER.getLastIndex(buf.substring(1));
            			if ( wk != -1 ) {
            				//抜いた分追加
            				index = wk+1;
            				suf = index;
            				t = Value.INVOKER;
            			}
            		} else if ( t.equals(Value.IDENTIFIER) ) {
            			int wk = Value.INVOKER.getLastIndex(buf);
            			if ( wk != -1 ) {
            				//抜いた分追加
            				index = wk;
            				suf = index;
            				t = Value.INVOKER;
            			}
            		} else if ( t.equals(Value.STRING) ) {
            			//ダブルコーテーションを排除
            			pre = 1;
            			suf = index - 1;
            		} else if ( t.equals(Operator.GT) ) {
            			// GEと解析を間違う場合がある為、処理を行う
            			int wk = Operator.GE.getLastIndex(buf);
            			if ( wk != -1 ) {
            				index = wk;
            				suf = index;
            				t = Operator.GE;
            			}
            		} else if ( t.equals(Operator.LT) ) {
            			// LEと解析を間違う場合がある為、処理を行う
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
            		} else if ( t.equals(Value.INTEGER) ) {
            			int wk = Value.REAL.getLastIndex(buf);
            			if ( wk != -1 ) {
            				index = wk;
            				suf = index;
            				t = Value.REAL;
            			}
            		} else if ( t.equals(Operator.ASSIGN) ) {
            			int wk = Operator.EQ.getLastIndex(buf);
            			if ( wk != -1 ) {
            				index = wk;
            				suf = index;
            				t = Operator.EQ;
            			}
            		}

            		//他はそのまま
            		String val = buf.substring(pre, suf);
            		Token token = new Token(t,val);
            		
            		logger.fine(token.toString());
            		
            		//トークンの終端を設定する
            		this.position += index;
            		token.setPosition(this.position);

            		rtn.add(token);
            		buf = buf.substring(index);
            		notfound = false;
            		break;
            	}
        	}

			if ( notfound ) {
           		Token token = new Token(Control.LEXER_ERROR,"Token Not Found");
           		token.setPosition(this.position);
				throw new LexerException(this,token,"Token Not Found");
			}
			buf = skip(buf);
		}
	
		//TODO ０件の場合エラー処理

        return rtn;
	}	

	/**
	 * スキップ
	 * @param buf 対象文字列
	 * @return トリムした文字列
	 */
	private String skip(String buf) {
		String rtn = buf.trim();
		//削除した文字列数を位置に足し込む
		int trim = buf.length() - rtn.length();
	
		this.position += trim;
		return rtn;
	}	
	
	/**
	 * プログラム文字列の取得
	 * @return
	 */
	private String getValue() {
		return value;
	}

	/**
	 * 字句解析後のトークンを取得
	 * @return
	 */
	public List<Token> getTokenList() {
		return tokenList;
	}

	/**
	 * 字句解析例外
	 * @author secon
	 */
	public class LexerException extends RuntimeException {
		private static final long serialVersionUID = 1L;
		public LexerException(Lexer lex,Token token,String string) {
			super(lex.debugLine(token, string));
		}
	}

	/**
	 * エラー位置のマーク
	 */
	private static final String Mark = "[*]";
	
	/**
	 * エラー時に原文の位置を表示
	 * <pre>
	 * 解析中にTokenには位置を埋め込んでいる為、エラー発生時に
	 * そのトークンの位置を文字列化する
	 * </pre>
	 * @param token 対象トークン
	 * @param msg メッセージ
	 * @return 位置などを追加したエラー情報
	 */
	public String debugLine(Token token, String msg) {

		if ( token.isType(Control.EOT) ) {
			//取得できなかった為、メッセージをそのまま返す
			return token + ":" + msg;
		}

		int tokenPos = token.getPosition();

		StringReader sr = new StringReader(this.getValue());
		int targetRow = 0;
		String targetLine = "";
		int row = 1;
		int col = 0;
		int pos = 0;

		try  (BufferedReader reader = new BufferedReader(sr) ) {
		    String line = null;
		    while ((line = reader.readLine()) != null) {
		    
		    	int lg = line.length();
		    	pos += lg;
		    	//位置を超えた場合
		    	if ( targetRow == 0 && pos >= tokenPos ) {

		    		col = lg - (pos - tokenPos);
		    		//行を設定
		    		targetRow = row;

		    		//マーク付きの文字列を作成
		    		CharSequence linePre = line.substring(0, col);
		    		CharSequence lineSuf = line.substring(col);
		    		targetLine = linePre + Mark + lineSuf;
		    		break;
		    	}
		    	//改行分足しておく
		    	pos = pos + lineIndex;
		    	row++;
		    }
		} catch (IOException e) {
			//取得できなかった為、メッセージをそのまま返す
			return token + ":" + msg;
		}

		//対象行を作成
		return String.format("%s\n[%d,%d] %s", msg,targetRow,col,targetLine);
	}
}
