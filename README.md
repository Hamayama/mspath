# mspath

## 概要
- Gauche の REPL 上で、Windows のパス名を文字列に変換するモジュールです。  
  ただし、パス名は、ダブルクォートではなくて '( ) で囲って渡す必要があります。

- Windows のコマンドプロンプトで Gauche の REPL を実行している場合、  
  .scm ファイル等を、マウスでドラッグ&ドロップすると、パス名が表示されます。  
  しかし、パス名の区切りの円記号 (もしくはバックスラッシュ) は、  
  文字列のエスケープ記号と判断されるため、うまく読み込むことができません。  
  本モジュールは、このパス名を 使用可能な文字列に変換するためのものです。


## インストール方法
- mspath.scm を Gauche でロード可能なフォルダにコピーします。  
  (例えば (gauche-site-library-directory) で表示されるフォルダ等)


## 使い方
```
  (use mspath) ; モジュールをロードします
  (mspath '(c:\work\aaa.txt))
               ; Windows のパス名(※)を文字列に変換します
  (msload '(c:\work\bbb.scm))
               ; Windows のパス名(※)でロードを行います
  (msrun  '(c:\work\ccc.scm))
               ; Windows のパス名(※)でロードを行い、main 手続きがあれば実行します

  (※)パス名は、ダブルクォートではなくて '() で囲って渡す必要があります。
      例えば、以下のように変換されます。
        '(c:\work\aaa.txt) → "c:\\work\\aaa.txt"
```


## 注意事項
1. 内部では、シンボルのリストを、1個ずつ文字列に変換しています。  
   この方法では、うまく変換できない文字が存在する可能性があります。

2. Gauche の REPL で、パス名に空白を含むファイルをドラッグ&ドロップした場合には、  
   パス名がダブルクォートで囲われて表示されます。  
   この場合は、ダブルクォートを手作業で削除する必要があります。


## 環境等
- OS
  - Windows 8.1 (64bit)
- 言語
  - Gauche v0.9.4
  - Gauche v0.9.5_pre1

## 履歴
- 2016-4-21 v1.00 (初版)
- 2016-4-21 v1.01 コメント修正等


(2016-4-21)
