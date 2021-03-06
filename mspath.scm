;; -*- coding: utf-8 -*-
;;
;; mspath.scm
;; 2017-10-15 v1.15
;;
;; ＜内容＞
;;   Gauche の REPL 上で、Windows のパス名をそのまま読み込むためのモジュールです。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/mspath
;;
(define-module mspath
  (use gauche.version) ; version<=?用
  (use gauche.process) ; process-output->string用
  (use file.util)      ; home-directory用
  (use srfi-13)        ; string-trim-both用
  (export
    mspath    mscd    mspwd    msload    msrun
    msys-path msys-cd msys-pwd msys-load msys-run
    ))
(select-module mspath)


;; パス名の入力を待つ(内部処理用)
(define (%read-line-path prompt)
  (display prompt) (flush)
  (if (version<=? (gauche-version) "0.9.4") (read-line))
  (read-line))

;; main 手続きを実行する(内部処理用)
(define (%run-main args)
  (if (global-variable-bound? 'user 'main)
    (with-module user (main args))
    0))


;; Windows のパス名を文字列に変換する
;;   ・パス名は、ダブルクォートではなくて '() で囲って渡す必要がある。
;;     例えば、以下のように変換される。
;;       '(c:\work\aaa.txt) → "c:\\work\\aaa.txt"
;;   ・パス名を省略すると、入力待ちになる。このときは '() は入力不要。
(define (mspath :optional (path-data #f) :key (prompt "path : "))
  (rlet1 path-str
      (cond
       ((list? path-data)
        (let1 space-flag #f
          (fold
           (lambda (p1 path)
             (let1 str (x->string p1)
               (cond
                ((or (string? p1) (#/^[\s\x3000;]*$/ str))
                 (set! space-flag #f)
                 (string-append path str))
                ((or (#/^\\/ str) (not space-flag))
                 (set! space-flag #t)
                 (string-append path str))
                (else
                 (set! space-flag #t)
                 (string-append path " " str)))))
           ""
           path-data)))
       ((not path-data)
        (%read-line-path prompt))
       (else
        (error "list or #f required, but got" path-data)))
    (set! path-str (regexp-replace-all* path-str #/\"/ "")) ; GitHubの色表示対策 "))
    (set! path-str (string-trim-both path-str))
    ;(write path-str) (newline) (flush)
    ))

;; mspath でパス名を変換後、cd を行う
;;   ・パス名が空のときは、ホームディレクトリに移動する
(define (mscd :optional (path-data #f) :key (prompt "path : "))
  (let1 path-str (mspath path-data :prompt prompt)
    (sys-chdir (if (equal? path-str "")
                 (home-directory)
                 path-str))
    (sys-getcwd)))

;; pwd を行う (sys-getcwd の単なるエイリアス)
(define mspwd sys-getcwd)

;; mspath でパス名を変換後、ロードを行う
(define (msload :optional (path-data #f) :key (prompt "file : "))
  (load (mspath path-data :prompt prompt)))

;; mspath でパス名を変換後、ロードを行い、main 手続きを実行する
(define (msrun :optional (path-data #f) (args '()) :key (prompt "file : "))
  (load (mspath path-data :prompt prompt))
  (%run-main args))


;; MSYS のパス名を Windows のパス名に変換する
;;   ・パス名は、ダブルクォートで囲って渡すこと。
;;     例えば、以下のように変換される。
;;       "/c/work/aaa.txt" → "C:\\work\\aaa.txt"
;;   ・パス名を省略すると、入力待ちになる。このときは、ダブルクォートは入力不要。
;;   ・外部プログラムの cygpath が必要。
(define (msys-path :optional (path-data #f) :key (prompt "path : "))
  (rlet1 path-str
      (cond
       ((string? path-data)
        path-data)
       ((not path-data)
        (%read-line-path prompt))
       (else
        (error "string or #f required, but got" path-data)))
    (set! path-str (regexp-replace-all* path-str #/\"/ "" #/'/ "")) ; GitHubの色表示対策 "))
    (set! path-str (string-trim-both path-str))
    (unless (equal? path-str "")
      (set! path-str (process-output->string
                      `("cmd.exe" "/c" "cygpath" "-w" ,path-str))))
    ;(write path-str) (newline) (flush)
    ))

;; msys-path でパス名を変換後、cd を行う
;;   ・パス名が空のときは、ホームディレクトリに移動する
(define (msys-cd :optional (path-data #f) :key (prompt "path : "))
  (let1 path-str (msys-path path-data :prompt prompt)
    (sys-chdir (if (equal? path-str "")
                 (home-directory)
                 path-str))
    (sys-getcwd)))

;; pwd を行う (sys-getcwd の単なるエイリアス)
(define msys-pwd sys-getcwd)

;; msys-path でパス名を変換後、ロードを行う
(define (msys-load :optional (path-data #f) :key (prompt "file : "))
  (load (msys-path path-data :prompt prompt)))

;; msys-path でパス名を変換後、ロードを行い、main 手続きを実行する
(define (msys-run :optional (path-data #f) (args '()) :key (prompt "file : "))
  (load (msys-path path-data :prompt prompt))
  (%run-main args))


