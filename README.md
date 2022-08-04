この計算器は外部からの変数を利用できる計算機です。

$test という変数を利用する場合、

```
String program = "return $test.toString()";
Variable var = new Variable();
var.add("test",obj);
Caluculator cal = Caluculator.create(var);
cal.eval(program);
```

という風に実行できます。
オブジェクトのメソッドを呼び出すことも可能で、動的なプログラムでオブジェクトを変更したりできます。

詳しくはテストケース見てください。

# 文末

基本的にはセミコロンを書きます。ただしエラーにはしていません。
改行に特に意味を持たせてない為、書かないとトークンが乱れて動かないかな？

# 内部変数

スコープは基本的に存在しませんが、letでの宣言がないと未定義扱いで終了します。

```
let hoge = "test";
```

# 予約語

- true 
- false 
- null
- if 
- return
- let

# if文

else には現状対応していません。

条件式はカッコ()で括ります、実行文は{}で括ります。


# 関数呼び出し

複数引数の呼び出しに現在対応してない為、対応が必要
