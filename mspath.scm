;; -*- coding: utf-8 -*-
;;
;; mspath.scm
;; 2016-5-29 v1.08
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
    mspath    mscd    mspwd    msload    msrun
    msys-path msys-cd msys-pwd msys-load msys-run
    ))
(select-module mspath)


;; パス名の入力を待つ(内部処理用)
(define (%read-line-path :optional (prompt #f))
  (display (or prompt "file : ")) (flush)
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
(define (mspath :optional (path-data #f) (prompt #f))
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
        (%read-line-path prompt))
       (else
        (errorf "list or #f required, but got ~s" path-data)))
    (set! path-str (regexp-replace-all* path-str #/\"/ "")) ; GitHubの色表示対策 "))
    (set! path-str (string-trim-both path-str))
    ;(write path-str) (newline) (flush)
    path-str))

;; mspath でパス名を変換後、cd を行う
(define (mscd :optional (path-data #f))
  (let1 path-str (mspath path-data "path : ")
    (unless (equal? path-str "")
      (sys-chdir path-str))))

;; pwd を行う (sys-getcwd の単なるエイリアス)
(define mspwd sys-getcwd)

;; mspath でパス名を変換後、ロードを行う
(define (msload :optional (path-data #f))
  (load (mspath path-data)))

;; mspath でパス名を変換後、ロードを行い、main 手続きを実行する
(define (msrun :optional (path-data #f) (args '()))
  (load (mspath path-data))
  (%run-main args))


;; MSYS のパス名を Windows のパス名に変換する
;;   ・パス名は、ダブルクォートで囲って渡すこと。
;;     例えば、以下のように変換される。
;;       "/c/work/aaa.txt" → "C:\\work\\aaa.txt"
;;   ・パス名を省略すると、入力待ちになる。このときは、ダブルクォートは入力不要。
;;   ・外部プログラムの cygpath が必要。
(define (msys-path :optional (path-data #f) (prompt #f))
  (let1 path-str
      (cond
       ((not path-data)
        (%read-line-path prompt))
       (else
        (x->string path-data)))
    (set! path-str (regexp-replace-all* path-str #/\"/ "" #/'/ "")) ; GitHubの色表示対策 "))
    (set! path-str (string-trim-both path-str))
    (unless (equal? path-str "")
      (set! path-str
            (receive (out tempfile) (sys-mkstemp (format "~a/mspath-cygpath" (sys-tmpdir)))
              (unwind-protect
                  (begin
                    (close-output-port out)
                    (sys-system (format "cygpath -w '~a' > ~a" path-str tempfile))
                    (with-input-from-file tempfile read-line))
                (sys-unlink tempfile)))))
    ;(write path-str) (newline) (flush)
    path-str))

;; msys-path でパス名を変換後、cd を行う
(define (msys-cd :optional (path-data #f))
  (let1 path-str (msys-path path-data "path : ")
    (unless (equal? path-str "")
      (sys-chdir path-str))))

;; pwd を行う (sys-getcwd の単なるエイリアス)
(define msys-pwd sys-getcwd)

;; msys-path でパス名を変換後、ロードを行う
(define (msys-load :optional (path-data #f))
  (load (msys-path path-data)))

;; msys-path でパス名を変換後、ロードを行い、main 手続きを実行する
(define (msys-run :optional (path-data #f) (args '()))
  (load (msys-path path-data))
  (%run-main args))


