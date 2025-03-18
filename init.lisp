
;;#+:allegro (excl::set-case-mode :case-sensitive-lower)

(defparameter *mma-files*
  '(;;"mma"  ;; problem loading after compiling???
    "uconsalt" ;; #+:allegro "ucons1"
    "parser"
    "stack1"
    "disp1"
    "poly"
    "rat1"
    "simp1"
    "pf"
    "eval"
    "newmatch"
    "diffrat"
    "morefuns"
    "function"
    "mma2maxfun"
    "batch"
    ))
(defparameter *mma-dir* "c:/lisp/mma4max/")

(defparameter *mma-source-files*
    (mapcar #'(lambda(r)(concatenate 'string *mma-dir* r ".lisp")) *mma-files*))


#+CLISP 
(defparameter *mma-object-files*
    (mapcar #'(lambda(r)(concatenate 'string *mma-dir* r ".fas")) *mma-files*))
#+(or allegro sbcl)
(defparameter *mma-object-files*
    (mapcar #'(lambda(r)(concatenate 'string *mma-dir* r ".fasl")) *mma-files*))

(defun compile-mma ()
  #+sbcl (sb-posix::chdir "c:/lisp/mma4max")  ;;; 3/8/2019 rjf doesn't work..
  (load "c:/lisp/mma4max/mma.lisp") ;; establish mma package
  ;; load files with macros used elsewhere .. defs that might matter to compiler
 (load "c:/lisp/mma4max/uconsalt.lisp")
    (load "c:/lisp/mma4max/poly.lisp")	; establish some macros for compiler
   (load "c:/lisp/mma4max/rat1.lisp")
   (map nil #'compile-file *mma-source-files*))

#+ignore
(defun compile-mma ()
  #+sbcl (sb-posix::chdir "c:/lisp/mma4max")  ;;; 3/8/2019 rjf doesn't work..
 ;; (load "mma.lisp") ;; 
  ;; load files with macros used elsewhere .. defs that might matter to compiler
  #+:allegro(load "ucons1.lisp")
  #-:allegro(load "uconsalt.lisp")

   (load "poly.lisp")			; establish some macros for compiler
    (load "rat1.lisp")
       
  (map nil #'compile-file *mma-source-files*))

;; make sure all dependencies taken care of
(defun recompile-mma()(dotimes (i 2)(compile-mma)(load-mma)))


(defun load-mma()
  (load "c:/lisp/mma4max/mma.lisp") ;; establish mma package   ?? 
  (map nil #'load *mma-object-files*)
       (setf *package* (find-package :mma)))

(defun load-mma-lisp()
  (load "c:/lisp/mma4max/mma.lisp") ;; establish mma package   ?? 
  (map nil #'load *mma-source-files*)
       (setf *package* (find-package :mma)))



