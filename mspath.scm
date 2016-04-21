;; -*- coding: utf-8 -*-
;;
;; mspath.scm
;; 2016-4-21 v1.01
;;
;; ＜内容＞
;;   Gauche の REPL 上で、Windows のパス名を文字列に変換するモジュールです。
;;   ただし、パス名は、ダブルクォートではなくて '() で囲って渡す必要があります。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/mspath
;;
(define-module mspath
  (use srfi-13) ; string-trim-both用
  (export
    mspath msload msrun))
(select-module mspath)

;; Windows のパス名を文字列に変換する
;;   ・パス名は、ダブルクォートではなくて '() で囲って渡す必要がある。
;;     例えば、以下のように変換される。
;;       '(c:\work\aaa.txt) → "c:\\work\\aaa.txt"
(define (mspath path-data)
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
       (else
        (errorf "list required, but got ~s" path-data)))
    (set! path-str (string-trim-both path-str))
    ;(write path-str) (newline) (flush)
    path-str))

;; Windows のパス名でロードを行う
;;   ・パス名は、ダブルクォートではなくて '() で囲って渡す必要がある。
(define (msload path-data)
  (load (mspath path-data)))

;; Windows のパス名でロードを行い、main 手続きがあれば実行する
;;   ・パス名は、ダブルクォートではなくて '() で囲って渡す必要がある。
(define (msrun path-data :optional (args '()))
  (load (mspath path-data))
  (if (global-variable-bound? 'user 'main)
    (with-module user (if (procedure? main)
                        (main args)
                        0))
    0))

