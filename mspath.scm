;; -*- coding: utf-8 -*-
;;
;; mspath.scm
;; 2016-4-25 v1.05
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
    mspath msload msrun
    msys-path msys-load msys-run
    ))
(select-module mspath)


;; パス名の入力を待つ(内部処理用)
(define (%read-line-path)
  (display "file : ") (flush)
  (if (version<=? (gauche-version) "0.9.4") (read-line))
  (read-line))

;; main 手続きを実行する(内部処理用)
(define (%run-main args)
  (if (global-variable-bound? 'user 'main)
    (with-module user (if (procedure? main)
                        (main args)
                        0))
    0))


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
        (%read-line-path))
       (else
        (errorf "list or #f required, but got ~s" path-data)))
    (set! path-str (regexp-replace-all* path-str #/\"/ "")) ; GitHubの色表示対策 "))
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
  (%run-main args))


;; MSYS のパス名を Windows のパス名に変換する
;;   ・パス名は、ダブルクォートで囲って渡すこと。
;;     例えば、以下のように変換される。
;;       "/c/work/aaa.txt" → "C:\\work\\aaa.txt"
;;   ・パス名を省略すると、入力待ちになる。このときは、ダブルクォートは入力不要。
;;   ・外部プログラムの cygpath が必要。
(define (msys-path :optional (path-data #f))
  (let1 path-str
      (cond
       ((not path-data)
        (%read-line-path))
       (else
        (x->string path-data)))
    (set! path-str (regexp-replace-all* path-str #/\"/ "" #/'/ "")) ; GitHubの色表示対策 "))
    (set! path-str
          (receive (out tempfile) (sys-mkstemp (format "~a/mspath-cygpath" (sys-tmpdir)))
            (unwind-protect
                (begin
                  (close-output-port out)
                  (sys-system (format "cygpath -w '~a' > ~a" path-str tempfile))
                  (with-input-from-file tempfile read-line))
              (sys-unlink tempfile))))
    ;(write path-str) (newline) (flush)
    path-str))

;; MSYS のパス名でロードを行う
;;   ・パス名は、ダブルクォートで囲って渡すこと。
;;   ・パス名を省略すると、入力待ちになる。このときは、ダブルクォートは入力不要。
;;   ・外部プログラムの cygpath が必要。
(define (msys-load :optional (path-data #f))
  (load (msys-path path-data)))

;; MSYS のパス名でロードを行い、main 手続きを実行する
;;   ・パス名は、ダブルクォートで囲って渡すこと。
;;   ・パス名を省略すると、入力待ちになる。このときは、ダブルクォートは入力不要。
;;   ・外部プログラムの cygpath が必要。
(define (msys-run :optional (path-data #f) (args '()))
  (load (msys-path path-data))
  (%run-main args))


