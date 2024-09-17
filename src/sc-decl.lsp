;;; Copyright (c) 2008 Tasuku Hiraishi <hiraisi@kuis.kyoto-u.ac.jp>
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

;;; sc-decl.lsp: definition of packages, readtables, constants and compilation environments

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide "SC-DECL")

(defpackage "SC-REQUIRE"
  (:nicknames "SCR")
  (:use "CL")
  (:shadow cl:require)
  (:export :*sc-system-path* :*auto-compile* :require :*cl-implementation* :*cl-version*))

(in-package "SC-REQUIRE")

;;;;;;
(defconstant *cl-implementation* 
  (progn "unknown"
     #+allegro "acl"
     #+cmu     "cmucl"
     #+sbcl    "sbcl"
     #+clisp   "clisp"))
(defconstant *cl-version*
    (with-output-to-string (s)
      (loop for ch across (lisp-implementation-version)
          if (char= #\Space ch) do (return)
          else do (write-char ch s))))

#+clisp
(defun gethostname () (let ((s (machine-instance))) (string-right-trim "." (subseq s 0 (position #\Space s)))))
#+(and allegro (not mswindows))
(defun gethostname () (excl.osi:gethostname))
#+(and allegro mswindows)
(defun gethostname () (sys:getenv "COMPUTERNAME"))

#+(or clisp (and allegro (not mswindows)))
(defun user-savepath () (user-homedir-pathname))
#+(and allegro mswindows)
(defun user-savepath () (format nil "~A\\" (sys:getenv "APPDATA")))

(defvar *sc-system-path* (make-pathname :directory (pathname-directory *load-pathname*)))
(defvar *auto-compile* t)
(defconstant *lisp-file-type* "lsp")
(defconstant *fasl-file-type* (pathname-type (compile-file-pathname "dummy")))
(defconstant *fasl-path-base*           ; コンパイル済Lispファイルの置き場所
    (merge-pathnames (format nil (concatenate 'string #-(and allegro mswindows)"." "sc-fasl"
                                              "/~A/~A/~A/")
                             (gethostname) *cl-implementation* *cl-version*)
                     (truename (user-savepath))))
#+comment (defconstant *fasl-path-base* "/") ; fasls are saved in sc-system-path
(defconstant *fasl-dir-base* (pathname-directory *fasl-path-base*))
(assert (eq :absolute (car *fasl-dir-base*)))

#+comment
(when (and (not (equal "/" *fasl-path-base*))
           #+clisp (not (ext:probe-directory *fasl-path-base*))
           #+allegro (not (excl:probe-directory *fasl-path-base*)))
  (unless (yes-or-no-p "This system makes a directory ~S and saves compiled lisp files there. OK? (if you would like to change the location, type \"no\" and modify *fasl-path-base* defined in sc-decl.lsp) "
                       *fasl-path-base*)
    (throw :sc-decl-exception nil)))

;; dirがdir-baseのサブディレクトリか？
(defun subdirectory-p (dir dir-base)
  (and (>= (length dir) (length dir-base))
       (loop
           for x in dir
           as y in dir-base
           always (equal x y))))

;; (or lsp fasl)の保存場所->faslの保存場所
(defun lspdir2fasldir (dir)
  (assert (eq :absolute (car dir)))
  (if (subdirectory-p dir *fasl-dir-base*)
      dir
    (append *fasl-dir-base* (cdr dir))))
   
;; (or lsp fasl)の保存場所->lspの保存場所
(defun fasldir2lspdir (dir)
  (assert (eq :absolute (car dir)))
  (if (subdirectory-p dir *fasl-dir-base*)
      `(:absolute ,@(nthcdr (length *fasl-dir-base*) dir))
    dir))

;;; require for SC system
;;; module-name を string-downcase したものに .lsp 拡張子をつけたファイルを
;;; path-list の先頭から順に捜して読みに行く．
;;; compile-file 済みで，.lsp より新しい場合はコンパイル版を読みにいく
(defmacro require (&rest args)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (do-require ,@args)))

(defun do-require (module-name &optional 
                               (path-list (delete nil (list *load-pathname*
                                                            *compile-file-pathname*
                                                            *sc-system-path*)))
                               (lisp-file-type *lisp-file-type*)
                               (fasl-file-type *fasl-file-type*)
                               (fasl-path-base *fasl-path-base*))
  (when (atom path-list) (setq path-list (list path-list)))
  (let* ((dir-list (mapcar #'(lambda (x) (pathname-directory (truename x)))
                           path-list))
         (lisp-file (string-downcase (string module-name))))
    (dolist (dir dir-list
              (error "Source file for module ~S does not exist." module-name))
      (assert (eq :absolute (car dir)))
      (let* ((lsp-dir (fasldir2lspdir dir))
             (fasl-dir (lspdir2fasldir dir))
             (lisp-path (make-pathname
                         :name lisp-file :directory lsp-dir 
                         :type lisp-file-type))
             (lisp-date (if (probe-file lisp-path)
                            (file-write-date lisp-path) -1))
             (fasl-path (make-pathname
                         :name lisp-file :directory fasl-dir
                         :type fasl-file-type))
             (fasl-date (if (probe-file fasl-path)
                            (file-write-date fasl-path) -1)))
        (cond 
         ((> fasl-date lisp-date)
          (return (cl:require module-name fasl-path)))
         ((> lisp-date fasl-date)
          (ensure-directories-exist (make-pathname :directory fasl-dir))
          (return (if *auto-compile*
                      (cl:load (compile-file lisp-path :output-file fasl-path))
                    (cl:require module-name lisp-path))))
         (t nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define packages, dynamic variables, constants, and features

(when (find-symbol "READTABLE-CASE" "CL")
  (pushnew :readtable-case *features*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage "SC-MISC"
  (:use "CL")
  (:export 
   "WITH-FRESH-VARIABLES" "WITH" "WITH*" "WITH1"
   "IT" "AIF" "ACASE" "AWHEN" "AUNLESS" "AWHILE" "AAND" "ACOND"
   "COMMAND-LINE"
   "WITH-READTABLE" "WITH-PACKAGE"
   "FIND-PACKAGE2" "IMMIGRATE-PACKAGE"
   "*EOL-CONVENTION*" "READ-FILE" "READ-FILE-AS-STRING" "WRITE-FILE" "DO-TEXT-LINE"
   "DO-INPUT-STREAM-BUFFER" "INPUT-BUFFER-TO-OUTPUT" "INPUT-BUFFER-TO-STRING"
   "DIRECTORY+" "PATH-SEARCH" "CHANGE-FILENAME" "CHANGE-EXTENSION" "GET-EXTENSION" "FILE-NEWER"
   "NAME-NAMESTRING"
   "STRING-BEGIN-WITH" "STRING+" "STRING+-REC" "STRCAT"
   "MEMBER-FROM-TAIL" "ADD-ELEMENT" "STRING-REF" "ADD-STRING" "ADD-PAREN"
   "CHAR-INVERTCASE" "CASE-MIXED-P" "STRING-INVERTCASE" "STRING-FIRSTN"
   "STRING-LEFT-NTRIM" "STRING-LEFT-TRIM-IF" "STRING-RIGHT-TRIM-IF"
   "STRING-LEFT-TRIM-SPACE" "STRING-RIGHT-TRIM-SPACE"
   "STRING-LEFT-TRIM-NOTSPACE" "STRING-RIGHT-TRIM-NOTSPACE" "SUBSTITUTE-STRING"
   "SPLIT-STRING" "SPLIT-STRING-1" "STRING-CASE" "STRING-CASE-EAGER"
   "NTH-MULTIPLE-VALUE" "SWAP" "CAT-SYMBOL" "SYMBOL+" "SYMBOL=" "SYMBOL-OF-NAME"
   "SEPARATE-LIST" "PUSHS" "INSERT" "INSERT1" "FIRSTN" "SUBSTITUTE-N"
   "POSITION-REC" "ASSORT" "STABLE-ASSORT" "ASSORT-BOOL" "STABLE-ASSORT-BOOL"
   "FIND-MAX" "FIND-POP" "MEDIAN" "MKLIST" "APPEND1" "NCONC1"
   "LIST-LENGTH>=" "LIST-LENGTH=" "LIST-RANDOM-SELECT" "POWER-SET"
   "LIST-UNTIL" "LIST-UNTIL-IF" "HEAD-INTERSECTION" "INTEGER-LIST"
   "MAKE-ALL-COMB"
   "MEMBER-REC" "COMBINE-EACH-NTH" "CMPD-LIST" "WITH-ALIST"
   "MAKE-QUEUE" "EMPTY-QUEUE-P" "FRONT-QUEUE" "QUEUE-LIST"
   "INSERT-QUEUE" "DELETE-QUEUE" "FIND-DELETE-QUEUE"
   "LIST-TO-HASHTABLE" "ALIST-TO-HASHTABLE"
   "CONST-F" "CHECK-MAPCAR" "RMAPCAR" "MAP-ALL-ATOMS" "DO-ALL-ATOMS" "MAPPEND"
   "VWHILE" "TILL" "FOR"
   "MEMOIZE" "DEFMETHOD-MEMO" "QUOTED-P" "TAGGED-P" "TAGGER" "TAIL-TAGGER"
   "COMPOSE" "COMPOSE-REV" "COMPOSE-N"
   "F-AND" "F-OR" "FUN"
   "PREFIXED-FUNC" "SUFFIXED-FUNC" "ARGFIXED-FUNC"
   "QUERY" "QUERY-SELECT-LIST"
   "ERROR-INDENT" "IERROR" "IWARN" "IFORMAT" "*ERROR-INDENT-SPACES*" "*NO-IWARN*"
   "MACROEXPAND-REC" "RECEXPAND" "UNRECEXPAND" "RECEXPAND-CLEAN"
   "RECEXPAND-ABBREV" "RECEXPAND-UNABBREV" "RECEXPAND-ABBREV-CLEAN"
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage "SC-MAIN"
  (:use "CL" "SC-MISC")
  (:export "SC2C" "CC"))

(defpackage "SC"
  (:use)
  (:import-from "CL"  ; defmacroで使えないのは不便なので
                "&ALLOW-OTHER-KEYS" "&AUX" "&KEY" "&OPTIONAL"
                "&REST" "&BODY" "NIL"))

(do-external-symbols (fs (find-package "CL"))
  (let ((scfs (intern (symbol-name fs) "SC")))
    (cond
      ((special-operator-p fs)
       (eval `(defmacro ,scfs (&rest args)
        (cons ',fs args))) )
      ((fboundp fs)
       (setf (symbol-function scfs) (symbol-function fs)))
      (t nil))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage "SC-FILE"
  (:use "CL" "SC-MISC")
  (:export "WITH-SC-READ-ENVIRONMENT" "READ-SC-FILE" "WRITE-SC-FILE"
           "*CODE-PACKAGE*" "*CODE-READTABLE*"))

(in-package "SC-FILE")
;;;; SCソースを読むときのpackage
(defconstant *code-package* (find-package "SC"))
;;;; SCソースを読むときのreadtable
(defconstant *code-readtable*
  #+readtable-case
  (let ((temp (copy-readtable nil)))
    (setf (readtable-case temp) :invert)
    temp)
  #-readtable-case
  (copy-readtable nil))

#+readtable-case
(setf (readtable-case *readtable*) :invert)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage "SCPP"
  (:use "CL" "SC-MISC")
  (:export "SCPP" "INITIALIZE"))

(defpackage "SCPP-MACRO"
  (:use))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage "SC-TRANSFORMER"
  (:nicknames "SCT")
  (:use "CL" "SC-MISC")
  (:export
   ;; sct.lsp (obsoleted)
   ;; "SCT" "MP-TEST"
   ;; sct-rule2lsp.lsp
   "*RULE-PATH*" "RULE2LSP"
   "MAKE-MATCHING-EXP"
   "*FORCE-RECOMPILE*" "*COMPILE-RULELSP*"))

(in-package "SC-TRANSFORMER")
(defvar *rule-path*)
(defvar *pattern-readtable*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage "RULE"
  (:nicknames "RULE" "SC-TRANSFORMER-USER" "SCT-USER")
  (:use "SC-MISC" "CL")
  (:shadow cl:declaration)
  (:export
   ;; sct-defrule
   "RETURN-NO-MATCH" "THROW-NO-MATCH" "TRY-RULE"
   "ENTRY" "DEFAULT-HANDLER"
   "DEFINE-RULESET" "EXTEND-RULESET" "DEFRULE" "EXTENDRULE"
   "RULESET-PARAM" "CALL-NEXT-RULE"
   "REQUIRE-RULESET" "APPLY-RULE"
   ;; sct-macro
   "IF-MATCH" "WHEN-MATCH" "UNLESS-MATCH" "COND-MATCH"
   ;; sct-lib.lsp
   "PATTERN-VARIABLE-P" "IF-PATTERN-VARIABLE" "GET-RETVAL" "NEXT-PATTERN"
   "GET-ALL-IDENTIFIER" "CID-P"
   "WITH-SETUP-GENERATE-ID" "MAKE-ID" "GENERATE-ID"
   "REMOVE-TYPE-QUALIFIER" "DEREF-TYPE" "UNSIGNED-P"
   "*TYPE-SIZE-ALIST*" "*TYPE-RANK-ALIST*"
   "*SIGNED-TO-UNSIGNED-ALIST*" "*FLOAT-TYPE-LIST*"
   "TYPE-CONVERSION"
   "SPLICE"
   ))

(in-package "SC-TRANSFORMER-USER")
;; sct-lib.lsp
(defvar *type-size-alist*)
(defvar *type-rank-alist*)
(defvar *signed-to-unsigned-alist*)
(defvar *float-type-list*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage "C2SC"
  (:use "CL" "SC-MISC")
  (:shadow cl:declaration)
  (:export 
   ;; c2sc-parser.lsp
   "C2SC" "PIECE-C2SC" "*CLANG-PACKAGE*" "PCSC"))

;; Cから変換された identifierを internする package
(defpackage "CSYM" (:use))

(in-package "C2SC")
(defconstant *clang-package* (find-package "CSYM"))
