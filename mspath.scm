;; -*- coding: utf-8 -*-
;;
;; mspath.scm
;; 2016-4-23 v1.03
;;
;; ＜内容＞
;;   Gauche の REPL 上で、Windows のパス名をそのまま読み込むためのモジュールです。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/mspath
;;
(define-module mspath
  (use gauche.version)
  (use srfi-13) ; string-trim-both用
  (export
    mspath msload msrun))
(select-module mspath)

;; Windows のパス名を文字列に変換する
;;   ・パス名は、ダブルクォートではなくて '() で囲って渡す必要がある。
;;     例えば、以下のように変換される。
;;       '(c:\work\aaa.txt) → "c:\\work\\aaa.txt"
;;   ・パス名を省略すると、入力待ちになる。このときは '() は入力不要。
(define (mspath :optional (path-data #f))
  (let1 path-str
      (cond
       ((list? path-data)
        (string-join
         (map (lambda (d)
                (if (string? d)
                  (errorf "list element must not be string, but got ~s" d))
                (let1 str (x->string d)
                  (if (#/^\\/ str)
                    str
                    (string-append " " str))))
              path-data)
         ""))
       ((not path-data)
        (display "file : ") (flush)
        (if (version<=? (gauche-version) "0.9.4") (read-line))
        (regexp-replace-all #/\"/ (read-line) "")) ; GitHubの色表示対策=> ")
       (else
        (errorf "list or #f required, but got ~s" path-data)))
    (set! path-str (string-trim-both path-str))
    ;(write path-str) (newline) (flush)
    path-str))

;; Windows のパス名でロードを行う
;;   ・パス名は、ダブルクォートではなくて '() で囲って渡す必要がある。
;;   ・パス名を省略すると、入力待ちになる。このときは '() は入力不要。
(define (msload :optional (path-data #f))
  (load (mspath path-data)))

;; Windows のパス名でロードを行い、main 手続きを実行する
;;   ・パス名は、ダブルクォートではなくて '() で囲って渡す必要がある。
;;   ・パス名を省略すると、入力待ちになる。このときは '() は入力不要。
(define (msrun :optional (path-data #f) (args '()))
  (load (mspath path-data))
  (if (global-variable-bound? 'user 'main)
    (with-module user (if (procedure? main)
                        (main args)
                        0))
    0))

