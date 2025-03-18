;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-o
;; Mathematica(tm)-like evaluator
;; copyright (c) 1990 Richard J. Fateman; pieces by Tak Yan, Derek Lai
;; hacked more 2/2011 to make it work in GCL, not complete. Need to put 
;; two-case symbols within | |.
;; hacked more 3/10/2019 to make it work with SBCL maxima


;;(eval-when (:compile-toplevel) (load "mma"))
(in-package :mma)
;;(provide 'math-eval)(require "ucons1")(require 'math-parser "parser")
;;(require "stack1") (require "disp1")(require "pf")(require "simp1")(require "rat1")(require "match")
(in-package :mma)

(defvar COUNT 1 "line number for top-level. (same as (meval |$Line|))")
(declaim (special env *expand* *numer*)) ;; environment

;;      funnyvars is a hash table containing variables which, when set,
;;      cause function to be executed

(defvar funnyvars (make-hash-table :test #'eq :size 8))
(defvar emptyht (make-hash-table))
;; We entera variable in the symbol table by making a hash
;; table as its value using chash.

(defvar symtab (make-hash-table :test #'eq :size 150))

(defun today-string()
  (multiple-value-bind
      (sec min hr date mo year day daylight-p zone)
      (get-decoded-time)
      (declare (ignore sec daylight-p zone))
    (let ((dayx (elt '(|Mon| |Tues| |Wednes| |Thurs| |Fri| |Satur| |Sun|) day))
	  (mox (elt'(|Jan| |Feb| |Mar| |Apr| |May| |Jun| |Jul| |Aug| |Sep| |Oct| |Nov| |Dec|) (1- mo))))
    
    (format nil "~a:~2d ~aday, ~a ~s, ~s" hr min dayx mox date year))))

(defun tl () ;; top level
  (let*
      ((*package* (find-package :mma))	h hs (hin  t)
       (timesofar 0)
       (timeunit (/ 1.0 internal-time-units-per-second))
       (env (make-stack :size 50))	;environment for matching binding
       )
    (declare (special env *package*))
    (initialize-mma)
    (if (= COUNT 1)
	(format t 
		"Mock-Mma  ~a  ~%" (today-string)))
    (loop
     (setq timesofar (get-internal-run-time))
      
    ;;; See beginning of mma.lisp (defun fo ....
    ;;;-kfp
      ;(if (eql hin '|Null|) nil  
	  ;(let nil(format t "~%In[~s] := " COUNT)
      ;    (finish-output t))) ;; actually In and Out are variables too.

     ;;;+kfp
     (if (eql hin '|Null|) nil (format t "~%In[~a] := ~/fo/" COUNT nil))
    
    ;;;

      ;; get the input
     (setq hin  (handler-case (mma::p) (error(x)(format t "~%syntax error ~s" x)(clear-input t) '|Null|)))
     (unless (eql hin '|Null|)      
       ;; evaluate it
       (setq h   (handler-case (meval hin)(error(x) (clrs) ;clear stack frames
						(format t "~%evaluation error ~s" x)`(|Hold| , hin))))
      
       (|SetQQ| (ulist '|In| COUNT) hin)

       (setq timesofar (- (get-internal-run-time) timesofar))
       ;; this is not the same as mathematica but I find it more convenient
       ;; this way. We've also implementing "Timing", if you prefer.
       (if (eq '|True| (meval '|$Showtime|))
	   (format t "~%time = ~3,$ secs.~%" (* timesofar timeunit)))
       ;; test for exit from top level to lisp REPL
       (cond ((or (member h '(EXIT |Exit|) :test 'eql)
		  (and (listp h)(eq (car h) '|Quit|))) ;;Quit[]
	      (format t"~%Exited to Lisp~%")
	      (return t)))
       (cond((eq h '|Null|) nil)
	    ;; don't print nil-valued answers
	    (t
	     (setq hs (list '|Set| (ulist '|Out| COUNT) h))
	     (disp (BuildFormat hs))))
       ;; regardless of printing of answer, set Out[]
       (|SetQQ| (ulist '|Out| COUNT) h)
       (|Set|  '|$Line| (incf COUNT))))	; loop end
    ))
;; this definition replaces the program in the parser file

(defun mread1()
  (cond((member (pc)'( #\space #\tab #\page) :test #'eql)
	(rc)(mread1))
       ((digit-char-p (mma::pc));; next character is a digit 0-9
	(mma::collect-integer 
	  (mma::char-to-int(read-char stream)) 10)) ;radix 10 default
	;; for every alphabetic symbol, set up a hash table
       (t (chash #-gcl(or(read-preserving-whitespace stream nil 'e-o-l) '|False|)
  	     #+gcl (recase(or(read-preserving-whitespace stream nil 'e-o-l) '|False|)))
	   ;; nil reads as False
	   )))



#|
We currently make each symbol table entry  out of a hash table.
 It's plausible to change this to use defstruct ... then
 make every declared "symbol-table-entry"  a structure
 with (at least) the following data  
(a) value for the symbol  e.g.  a=3 in the value cell
(b) value for expressions with the symbol as head. e.g. a[45]=x+1
   we might have different "arities" e.g. a[45] has arity 1,
   a[3,4] has arity=2, etc. [we don't use this now]
(c) value for the collected attributes of the symbol.
       e.g. Attributes[a] ={Orderless, Protected, Listable}
(d) value for each of the attributes to make access fast with using member-test
  on collected value  [we don't use this now]
(e) value for function definition "built-in"  e.g. numeric cosine
(f) value for user-defined function rules e.g. a[x_]:= ...
   we could again use some "arity" discrimination if we expect
   function definitions of different numbers of arguments. [we don't use this
   now]
(g) string for symbol printing (e.g. " + " for Plus). Except that
  so far the Lisp symbol-name for Plus is Plus, so we don't need this.
(h) formatting information. right now the List symbol-table entry e.g.
  for Plus has the Plus formatter program stored
(i) left/right operator precedence information; display stores this as above
(k) derivative and integral info, as above
(j) messages/ documentation
(k) package? context?

If we were to revise all this we could be more specific
about possible types for the fields,
e.g. for some of these..  (c): list; (d) bit-vector;
(e) lisp-function-value; (f) list? array? (g) string; (h) program,
(k) pattern or program

|#
(defun chash(m)
  (let ((*package* (find-package :mma)))
  (cond
   ((not(symbolp m))m)
;;   ((null m)nil) do we need to check for nil or t? Maybe not.
   (t (cond((gethash m symtab)) ;either it's there or
	   (t(setf (gethash m symtab)
		   (make-hash-table :test #'equal :size 4)); we make a hashtable

	     ))))
  m))

;; the following stuff is make-shift.

(defun |Head| (h)(typecase h
		 (cons (car h))
		 (ratio '|Rational|)
		 (complex '|Complex|)
		 (integer '|Integer|)
		 ;((fixnum bignum integer) '|Integer|)
					;((double-float single-float) '|Real|)
		 (float '|Real|)
		 ;;((rat) 'rat) ;; mockmma rat function form, e.g. Rat[x+1].
		 ;; file descriptors? characters? graphics? arrays?
		 (string '|String| )
		 (symbol '|Symbol| )
		 (array '|Array|)
		 (rat '|Rat|) ;; special rational form
		 (otherwise  (type-of h) )
		 ))
		 
;; Assignment statements treat the lhs with partial evaluation.
;; For a non-atomic Head, evaluate all the arguments, but not
;; the head. Presumably this should check attributes of the Head
;; like |HoldAll|, |Hold|First, etc. We don't do that yet,
;; 12/2010
;; but probably must do |Hold|Rest at least for RuleDelayed so Rubi can work.
;; and Release, Release|Hold|.

;; To the extent possible, I have avoiding thinking about Mma's bogus version of
;; Quote and Eval for as long as I can. for decades?

;; we evaluate the lhs partially and then the rhs.

;; we'd like to have a Quote operator, but the repeated evaluation rule
;; makes it almost impossible to work unless we check for it specially..
;; alternatively, we can set *evalonce* to t, and (vastly)
;; change the semantics. Sometimes this vast change is no change at all....

;; We evaluate args, depending on the hold-specs of the head

;; The Mathematica semantics avoids repeated evaluation by putting a time stamp
;; on each object, as well as dependencies.  If the object is newer than any
;; thing it depends on, then it is already "evaluated".  Otherwise it
;; has to be re-evaluated and re-time-stamped. We don't do this right now,
;; but we might have to.  Mathematica is a little vague and dependencies are
;; not listed explicitly (we think), but approached approximately, e.g.
;; expression x+y+z depends on stuff on some pages (which include the
;; residences of x,y,z.  But they might all be on one page.  And there
;; may be other things on those pages that change, so x+y+z might be
;; unnecessarily re-evaluated.  In our scheme everything is re-evaluated,
;; almost inevitably way too much.

(defun mevalargs( ha l) ;; attributes of head
  ;;(format t "~%mevalargs env=~s" (describe env))
     (unsequence
     (cond ((member '|HoldAll| ha :test 'eq)l)
	   ((member '|HoldFirst| ha :test 'eq) 
	    (ucons (car l)(mapcar #'meval (cdr l))))
	   ((member '|HoldRest| ha :test 'eq) (ucons (meval (car l))(cdr l)))
	   (t ;;(format t "~%in mevalargs l=~s, res=~s" l  (umapcar #'meval l))
	       (umapcar #'meval l)))))


(defun unsequence(k) ;; change ( a b (|Sequence| c d) e)  to  (a b c d e)
  (cond ((null k) nil)
	((and (consp (car k))(eq (caar k) '|Sequence|))
	 (append (cdar k)(unsequence (cdr k))))
	(t (setf (cdr k)(unsequence (cdr k))) k)))


;; note that the name of this function conflicts with that
;; of the lisp function set, unless
;; (a) capitalization is observed  OR
;; (b) the package system is protecting it..

#+ignore
(defun |Set| (lhs rhs &aux h fun);; lhs=rhs
  
  ;; the value associated with the lhs will be stored
  ;; in the symbol table symtab, with the key h,
  ;;  which is either the head of the lhs,
  ;; or the lhs itself.  That is  f=45 is stored in the
  ;; hash table for f, under the indicator "f"
  ;; and f[0]=7 is stored in the hash table for f, under
  ;; the indicator (0).
  
  (cond ((symbolp lhs)(setq h lhs)) ;; simple case,  x=stuff
	(t (setq h (car lhs)) ;;  not so simple, x[i,j]= stuff, evaluate i,j
	   (setq lhs (mevalargs (|Attributes|(|Head| h))(cdr lhs)))))
  
  ;;(format t "Set ~s to ~s, h=~s~%" lhs rhs h)
  ;; this stores the material in the hash table.
  ;; QUESTION: M'ma doesn't do this, but we could, by storing
  ;; stuff on a local environment... f[x_,y_]:=Block[{},x=y+1];
  
  ;; what if (gethash h symtab) is a matrix, and this is a valid matrix
  ;; setting?  Then we should try to store the value in the array.
  ;; This is insufficient error checking but... 
    
  
  (cond     
	;;else  THIS happens, for a global variable.  We need to fix for local var
	;; on stack.
		     
	(t ;;(print 'xxxxx)
	   ;;(chash h)
	 (setf (gethash lhs (gethash h symtab)) rhs)
	   
	  ;; (setq rhs (meval rhs)) ;; hold it,  rhs is already evaluated
	   
	   ))
  ;; Next, check for special variables which, when set, cause other
  ;; things to happen. E.g. Precision= number means, in the
  ;; bigfloat system, (bigfloat-init number) gets executed.
  (if (setq h (gethash h funnyvars ))(funcall h rhs))
  rhs)

(defun |Set| (lhs rhs) ;; lhs=rhs
  ;; the value associated with the lhs will be stored
  ;; in the symbol table symtab, with the key h,
  ;;  which is either the head of the lhs,
  ;; or the lhs itself.  That is  f=45 is stored in the
  ;; hash table for f, under the indicator "f"
  ;; and f[0]=7 is stored in the hash table for f, under
  ;; the indicator (0).
  (declare (special symtab funnyvars))
  (let ((h nil)(fun nil))
    (cond ((symbolp lhs) ;; common case,  "x=stuff"
	   ;; 3 possibilities, x is bound on stack or or a funny-var or is global
	   ;; (format t "~%setting symbol ~s" lhs)
	   (multiple-value-bind (val found)(sfind env lhs)
	     ;; 	   (format t "~%mvb result for symbol  ~s is ~s ~s " lhs val found)
	     (declare (ignore val)) ; previous value irrelevant
	     
	     (cond (found		   ;found on stack.
		    (schange env lhs rhs)) ;;change it there.
		   ((setf fun (gethash lhs funnyvars)) ;one of the vars needing special work
		    ;;  (format t "~%~s is in funnyvars"~s)
		    (funcall fun rhs))
		   (t ;;global case. Not found on stack
		    ;; (format t "~%symtab is ~s" symtab)
		    (setf h (gethash lhs symtab)) ;odd. should be there from parser.
		    ;;  (format t "~%hashtable for ~s is ~s" lhs h)
		    (cond((null h)(chash lhs) (setf h (gethash lhs symtab))))
		    ;; set h to the hash table for "x"
		    (setf  (gethash lhs h) rhs))))) ;  put value on key "x" in "x" hashtable
	  
	  ;; not not so simple an assignment, e.g., x[i,j]= stuff,
	  ((consp lhs)
	   (cond ((eql (|Head| lhs) '|Part|)
		  ;; case of m[[3]]=45
		  ;; maybe also mm[[3,4]]=45.
		  ;;IMPORTANT: deviation from Mma spec. We return WHOLE expression
		  (setf rhs (uniq(setpart (append (meval (cadr lhs))nil) ;fresh copy
					  (cddr lhs)
					  rhs))) )
		 (t  ;; it is an assignment like f[1,2]=10
		  (setq h (gethash (car lhs) symtab)) ;symtab for x
		  (cond((null h)(chash (car lhs)) (setf h (gethash (car lhs) symtab))))
		  ;;	   (format t "~% Set has non-atom lhs ~s" lhs)
		  ;;	   (format t "~%hashtable for ~s is ~s" (car lhs) h)
		  (setq lhs (mevalargs (|Attributes|(car lhs))(cdr lhs))) ; evaluate i,j
		  (setf (gethash lhs h) rhs)) )))
	  rhs))			;return value

;; the following program has an insideous feature.
;; rr=f[a,b,c]
;; rr[[2]]=2
;; now rr is   f[a,2,c].  That's good
;; but g[a,b,c]
;; comes out as g[a,2,c]   because the single copy in memory of [a,b,c] has been
;; destructively altered to a,2,c.
;; How to fix this???
;; we changed the return value of rr[[2]]  so that it is the new value of rr.


;; this version changes the return value of part[m,1]=45 to return a new value for all of m,
;; not just 45.

(defun setpart(m locs val)
  (cond ((null (cdr locs)) ;; just one index
	 (setf (elt m (car locs)) val)
	 m)
	(t(setf (elt m (car locs))(append (elt m (car locs)) nil)) ;make fresh copy
	  (setpart (elt m (car locs))
		    (cdr locs)
		    val))))
  
  
  ;;(format t "Set ~s to ~s, h=~s~%" lhs rhs h)
  ;; this stores the material in the hash table.
  ;; QUESTION: M'ma doesn't do this, but we could, by storing
  ;; stuff on a local environment... f[x_,y_]:=Block[{},x=y+1];
  
  ;; what if (gethash h symtab) is a matrix, and this is a valid matrix
  ;; setting?  Then we should try to store the value in the array.

    ;; Next, check for special variables which, when set, cause other
  ;; things to happen. E.g. Precision= number means, in the
  ;; bigfloat system, (bigfloat-init number) gets executed.

;; there is another file  (nmatrix) that defines a matrix type..
;; this should be tied in to the matrix stuff from franz, perhaps.
;; also, we have to decide which bigfloat to use... mpfun or rjf's
;; old bfstuff.
;; or MPFR. 12/2010


(defun matrix-p(x) (declare (ignore x))nil) ;;; for now, this will have to do.

(defun |SetQQ|(lhs rhs &aux h);; lhs=rhs, but don't mevaluate either.
  
  (setq h(cond ((atom lhs) lhs)
	       (t (prog1 (car lhs) (setq lhs (cdr lhs))))))
  (setf (gethash lhs (gethash h symtab))rhs)
  
  (if (setq h (gethash h funnyvars ))(funcall h rhs))
  rhs)

(defun |Clear|(&rest xl)
    (map nil  #'clear1 xl)
  '|Null|)

(defun clear1(h)
  ;;  RuleHT --clear out rule optimization hash table that seems to have bugs.
  ;;  labels -- resets counter
  (declare (special COUNT optimruleht ))
  (cond ((eql h '|Labels|) (setf COUNT 0) (|Set|'|$Line| 0))
	((eql h '|RuleHT|)
	 (setf optimruleht (make-hash-table :test 'eq)))
	(t (let ((mm (gethash h symtab)))
	     (if mm (remhash h symtab) nil))))
  nil)
  

;; this hash table has the optimized version of the pretty rules in it
(defparameter  optimruleht (make-hash-table :test 'eq))

(defun |SetDelayed|(lhs rhs) ;; this is the function definition f[x_] := ...
  (let* ((visible (ulist '|SetDelayed|  lhs rhs))
	 (hidden (bindfix(fixopts visible))))
    (setdelayed1 hidden)
    (setf (gethash visible optimruleht) hidden) ;; store the optim rule/def here
    '|Null|))

(defun setdelayed1(thedef)
  (let* ((lhs (ucons (caadr thedef)
		     (umapcar #'meval (cdadr thedef)))) ;; see note re eval below
	 (hh (|Head| lhs)) ;; lets hope the Head is a symbol 
	 (spot (gethash hh symtab)))
;    (format t "~%lhs=~s" lhs)
    ;; should we evaluate the formal arguments?  we need to do something, e.g.
    ;; f[Sqrt[x_]]  should be changed to f[x_^(1/2)]
    ;; this could be merely simplification, not evaluation .. ugh.
   ;; (setf lhs (ucons (car lhs)(mevalargs nil (cdr lhs))))
	 ;;(args (mevalargs (|Attributes|(|Head| hh)) (cdr lhs-in)))
	 ;(args (cdr lhs-in)) ;; these should not be evaluated; they are like formal params, no?
    ;; push all but the 'SetDelayed  on the list of definitional forms.
    (if (null spot)(setf spot (setf (gethash hh symtab)  (make-hash-table :test #'equal :size 4)))) ; not needed except bug?
    (cond ((null (gethash '|SetDelayed| spot))
	   (setf (gethash '|SetDelayed| spot) (ulist (ucons lhs (cddr thedef)))))
	  (t (push (ucons lhs (cddr thedef)) (gethash '|SetDelayed| spot))))))
    

;; this assumes the value of a mathematica symbol is its lisp value
;; if it is simply a constant or non-hash-table. That means that
;; a lisp dynamically bound variable could be used to block access
;; to the globally bound variable of the same name.  Better not
;; use local-variable names like Sin or Cos unless you mean to
;; inhibit access to the global names.


(defun meval-atom(h)  
  (declare (special env *numer*))
  
  (if (member '|Constant| (|Attributes| h):test 'eq)
      (if *numer* (numer-meval h)h) ;; works for 3, 1/2 
    (multiple-value-bind 
	(val found)
	(sfind env h)
      (if found val ;; return val
	;; if we find it here on the env stack.
	;; otherwise
	;; val is a symbol, same as h  see if it has a value
	(let ((r (gethash val symtab)))
	  (if (hash-table-p r)
	      (gethash val r val) ; 
	    val))))))

(defun numer-meval(h) (case h 
			(|Pi| pi)  ;; this is just good for double float
			(|E| #.(exp 1.0d0))
			(otherwise (if(symbolp h) (or (get h 'numerval) h) h))))
 ;; uh, justa hack
			    ;; works for 3, 1/2 

;; look up the value of e on e's hashtable, key e
;; look up the value of e[10] on e's hashtable, key (10)

(defun msymbol-value (h) 
    (cond
     ((atom h) (meval-atom h))
     (t (let ((tabentry (gethash (|Head| h) symtab)))
	  (if tabentry (gethash (cdr h) tabentry h)
	    h)))))


;; this guy looks on the stack; returns if there. looks on global symtab; returns if there.
;; no loop. works, evaluates "once" if that's what you want.
(defun msymbol-function(h)
  ;; hm, how much work should we do here?
  ;; it could be that h is just h, and has no binding of any sort.
  ;; it returns either a non-function or a (global) SetDelayed list or rules

     (setf h (cond 
	      ;; if we think we should look on the stack for it, then we do this.
	      ;; if the value is yet another name, what then? Do we use that
	      ;; name or continue looking??
   
	      ((multiple-value-bind 
		   (val found)
		   (sfind env h)
		 (if found val nil))) ;; if the function is found on stack, it is val
	      ;; we go on to the next clause if the function is not found
	      ;; on the stack.
	      
	      ((multiple-value-bind 
		   (val found)
		   (gethash h symtab)
		 (if found (return-from msymbol-function (gethash '|SetDelayed| val h))
		   ;; if not found return h
		   h)))))
     h)

;; this guy may be useful, if you want to keep looking for the value...
;;for evaluating until no change.. 
#+ignore
(defun msymbol-function(h)
  ;; hm, how much work should we do here?
  ;; it could be that h is just h, and has no binding of any sort.
  (let ((saved h))
    (loop 
     (setf h (cond 
	      ;; if we think we should look on the stack for it, then we do this.
	      ;; if the value is yet another name, what then? Do we use that
	      ;; name or continue looking??
   
	      ((multiple-value-bind 
		   (val found)
		   (sfind env h)
		 (if found val nil))) ;; if the function is found on stack, it is val
	      ;; we go on to the next clause if the function is not found
	      ;; on the stack.
	      
	      ((multiple-value-bind 
		   (val found)
		   (gethash h symtab)
		 (if found (return-from msymbol-function (gethash '|SetDelayed| val h))
		   ;; if not found return h
		   h)))))
      (if (equal h saved) (return h) (setf saved h)))))


;;this version always goes for global function definition ONLY. not compatible with mma  ...
#+ignore
(defun msymbol-function(h)
  ;; hm, how much work should we do here?
  (multiple-value-bind 
       (val found)
       (gethash h symtab)
     (if found (return-from msymbol-function (gethash '|SetDelayed| val h))
       ;; if not found return h
       h)))

 ;; is this going to have the right scope?
;; 

;;----end of makeshift definitions

(defun mapply (hraw args expr env);; note that args= (cdr expr)
  (let* ((fun nil)
	 (h (meval-to-function hraw))
	 ;;(msymq (gethash h symtab))
	 )		
    
    ;; there are 2 kinds of applications here.
    ;; (Function ...)  which is just held.
    ;; (w ...)  where w is (Function ...)
 ;;     (format t "~% h=~s hraw=~s args=~s" h hraw args)
    
    (cond ((eql h '|Function|) 		       
	   (return-from mapply (ucons '|Function| (funfix (cdr expr)))))
	  ((and (consp h)(eql (car h) '|Function|))
	   ;; this next line should grab the attributes of this Function, if any
;;	   (format t "~%before args to function ~s are ~s, with env=~s" h args env)
	 ;;  (setf args (mevalargs (cdddr h) args))  will be done in mappfun
;;	   (format t "~%after args to function ~s are ~s" h args)	   	   
	   (return-from mapply (mappfun h args expr env))))
    
    ;; (format t "~%before args.. ~s are ~s, with env=~s" h args env)	
    ;;get info on evaluating arguments and do it to args
    (setf args (mevalargs (|Attributes| h) args)) 
    
    (cond
     ;; I don't believe the comment below...

     ((constantp h) 
     ;; (format t "~%h=~s" h)
					; h

    (setf expr  (cons h args))
      ) ;; allows any lisp function, almost, to slip through
     ;; check for constant values pre-stored
     ((not (symbolp h)) (setq expr(ucons h args)))
     
     ;; oo... this makes lisp function go first.  not right??
     #+ignore
     ((and				;(not msymq)
       (symbolp h)(fboundp h))
      ;(format t "~% applying a lisp function ~s to args ~s " h args )
      (setq expr (apply h args)))

     ;;((not msymq)(setq expr (ucons h args)))
     ;; maybe put in a check for array here? Not now.
     ;; next check for user-defined function
     ((consp (setq fun (msymbol-function h)))
      ;;(setq args (mevalargs h args)) ;; not always ..
     ;; (format t "~%applying a user function ~s to args ~s" fun args)
      (setq expr(rulesetapply h fun args)))
     ;; next check for built-in LISP function
     ;; (clearly not something that Mathematica does)
     
     ((and				;(not msymq)
       (symbolp h)(fboundp h))
      ;(format t "~% applying a lisp function ~s to args ~s " h args )
      (setq expr (apply h args	)))
   
     (t ;;(format t "~% eval fell through ~s ~s" h args)
	(setq expr(ucons fun args))))
 ;;(format t "~% mapply result: expr=~s" expr)
    expr))

;;(declaim (special phead)) ;; not needed?

;;#+ignore
(defun rulesetapply(phead rules args)
  ;; get attributes of phead and manipulate args
  (setq args (mevalargs (|Attributes|(|Head| phead)) args))
  (let* 
      ((origfn phead)
       (expr (ucons phead args)))
    ;; (if isflat nil (setq phead '|Sequence|))

    (do ((rr rules (cdr rr)))
	((null rr) ;; no more rules to try -- return original
	 expr)
                
      (let* ((thisrule (car rr))
	     (condit #'truth)
	     (lhs (car thisrule))
	     (rhs (cadr thisrule))
	     (testr nil))
	;; Note: if the rule was
	;; f[a_,b_]:= g[a,b] /; a>b, the parsed result is
	;; (SetDelayed (f ..) (Condition (g a b) (Greater a b)))
	;; see if there is a Condition on the rhs of the rule
	;; e.g. (Condition (foo a b) (Greater a b))

	;; deal with the possibility of a Condition coming in.
	(cond ((and (consp rhs)(eq (car rhs) '|Condition|))
	       (setf testr (caddr rhs))
	  ;;     (format t "~%try to match ~s with condition ~s" lhs testr)
	       (setf condit #'(lambda()	;(format t "~% evaluating condit ~s" testr)
				(meval testr))) ;eg testr = (Comparison a Greater b)
	       (setf rhs (cadr rhs))))
					;rhs = (foo a b)
	;;(format t "~%lhs= ~s ~%rhs= ~s expr=~s ~%condition =~s" lhs rhs expr condit)
	(if (not (eql (|Head| expr) origfn))(return-from rulesetapply expr)) ;no more rules here are relevant 
	;; test for matching
	;; REDONE 2/2011 RJF, 
	;;(spushframe env phead)

	(cond ((match lhs expr condit) ;; matching works
	       (setf expr(meval rhs)) ;; something like this.. 
	       (spopframe env)	      ;; pop off the match frame
	       (return-from rulesetapply expr)) ;; end of match, don't try more rules in do loop
	      
	      (t  (spopframe env)))
	))))


;;(defun falsenull(h)(or (null h)(eq h '|False|))) ;; test for mockmma False or maybe nil
;;(defun notfalse(h) (not (falsenull h)))  ;; anything not False or nil
;; Major evaluation function for an expression
;; see Mathematica book p 568
;;(defun meval-to-function(x) x) ;; don't evaluate function name ?
(defun meval-to-function(x) (meval x))
(defvar *evalonce* t)  ;; should be t to make quote (etc etc) work

(defun meval (e)
    (let ((saved e)(hd nil) (ha nil))
      (if(atom e)(return-from meval  (meval-atom e)))
      (cond ((eql (car e) 'lispapply)
	     #+ignore(format t "~% lispapply found, fun= ~s, args=~s" (cadr e) 
		     (let ((m (meval (caddr e))))
		       (if (consp m)(cdr m)(list m))))
	     
	     (return-from meval (apply (cadr e)  (let ((m (meval (caddr e))))
		       (if (consp m)(cdr m)(list m)))))))
      (if (eq (setf ha (msymbol-value e))e)	;didn't find a value
	  nil	(return-from meval ha))	; DID find a value.

      
      ;; check off other constant expressions that don't evaluate.
      ;; perhaps Messages?
      ;;((patternobjectp e) e) .. What about Patterns?
      ;; (mapply (car foo)(cdr foo)  foo env) ==> foo  with no conses...
      (setf e (cons (meval-to-function (|Head| e))(cdr e)))
      ;; (return-from meval (mapply (car e) (cdr e) e env))      ))
      (setf e (mapply (car e) (cdr e) e env))
      ;; note the 3rd arg to mapply, just in case you want to
      ;; return the expression as the result without any change.
      ;; next step --
      ;;
      ;; do we keep evaluating until there is no change???
      (setf hd (|Head| e))
      (setf ha (|Attributes| hd))

;;       (format t" ~%ha=~s hd=~s e=~s" ha hd e)

      (cond (*evalonce* e)
	    ((eql hd '|Function|) (funfix (cdr e)) e) ;; compute new body
	  
	    ((and (member '|Listable| ha) (some #'|ListQ| (cdr e)))
	     ;; uh, (h a,b,..(List c d) ..) becomes
	     ;; (List (h a b ..c..) (h a b .. d..)  ).
	     (listify e))
	     ((equal e saved) e)
	    ((eql hd '|Hold|)  e)
	    (t (meval e)))
      ))

(defun listify(e) 
  (let ((listlength nil)(tt 0))
    (loop for i in (cdr e) do
	  ;; scan for length
	  (cond ((|ListQ| i) 
		 (setf tt (length (cdr i)))
		 (if listlength (cond ((= tt listlength) nil )
				      (t (format t "~%objects of unequal length cannot be combined ~s" e)
					 (signal 'error)))
		   ;; no listlength yet. set it
		   (setf listlength tt)))))
    ;; now make a list of length listlength
(ucons '|List|    (loop for i from 1 to  listlength collect
	 (ucons (car e) (loop for j in (cdr e) collect (if (|ListQ| j)(elt j i) j))))))		)
		
		

;; Each global object X is associated with a hash table
;; and we can, for each, 
;; to get the value, do (gethash X X), (gethash 'rules X) etc.

;; Local bindings hide everything.

;;Do we want to do infinite evaluation? 
;;How do we keep single copies of items in memory?
;;set up initial symbol-table entries for built-in stuff
;; should also set attributes
(defvar *initialized-mma* nil)

(defun initialize-mma ()
  (unless *initialized-mma*
    #-:allegro (setupcases)
    (mapc #'chash built-in-syms)
    (globinit '|$Line| 1)
    (globinit '|False| nil) ;; maybe, maybe not
    (globinit '|$Showtime| nil)
    (globinit '|I| #c(0 1)) ;; imaginary unit  I^2 = -1.


    (setattribute '|Plus| '|Flat|)
    (setattribute '|Plus| '|Orderless|)
    (setattribute '|Plus| '|Default| 0)
    (setattribute '|Plus| '|Listable|)
    (setattribute '|Times| '|Flat|)
    (setattribute '|Times| '|Orderless|)
    (setattribute '|Times| '|Listable|)
    (setattribute '|Times| '|Default| 1)

    (setattribute '|Power| '|Default| 1)

    (setattribute '|And| '|HoldAll|)	; short-circuiting
    (setattribute '|Or| '|HoldAll|)
    (setattribute '|If| '|HoldRest|)
    (setattribute '|Condition| '|HoldRest|)

    (setattribute '|Set| '|HoldFirst|) 
    (setattribute '|Set| '|HoldSequence|) ;; whatever this does; nothing at the moment
    (setattribute '|SetDelayed| '|HoldAll|)
    (setattribute '|UpSet| '|HoldFirst|)
    (setattribute '|UpSetDelayed| '|HoldAll|)
    (setattribute '|TagSet| '|HoldFirst|)
    (setattribute '|TagSetDelayed| '|HoldAll|)
    (setattribute '|Pattern| '|HoldFirst|)
    (setattribute '|ReplaceRepeated| '|HoldRest|)
    (setattribute '|RuleDelayed| '|HoldRest|)
    (setattribute '|Clear| '|HoldAll|)
    (setattribute '|Do| '|HoldAll|)
    (setattribute '|Table| '|HoldAll|)
    (setattribute '|Every| '|HoldAll|)
    (setattribute '|Some| '|HoldAll|)
    (setattribute '|Function| '|HoldAll|)
    (setattribute '|Timing| '|HoldAll|)
    (setattribute '|AddTo| '|HoldFirst|)
    (setattribute '|Increment| '|HoldFirst|)
    (setattribute '|PreIncrement| '|HoldFirst|)
    (setattribute '|Decrement| '|HoldFirst|)
    (setattribute '|Pi| '|Constant|)
    (setattribute '|E|  '|Constant|)
    (setattribute '|RuleDelayed| '|HoldRest|)  
    
    (setf (gethash '|SetDelayed| (gethash '|Power| symtab)) powerrules)
    
    ;; do all the ratdiff stuff
    (setf (gethash '|SetDelayed| (gethash '|Int| symtab)) integraterules)
    (setf (get '|Exp| 'deriv) (make1dfun '(|Exp| %))) ;; or should it be (|Power| E %)???
    (setf (get '|Log| 'deriv) (make1dfun '(|Power| % -1))) ;; powersimp better than |Power|??
    (setf (get '|Sin| 'deriv) (make1dfun '(|Cos| %)))
    (setf (get '|Cos| 'deriv) (make1dfun '(|Times| -1 (|Sin| %))))
    (setf (get '|Tan| 'deriv) (make1dfun '(|Power| (|Sec| %) 2)))
    (setf (get '|Sec| 'deriv) (make1dfun '(|Times| (|Sec| %) (|Tan| %))))
    (setf (get '|Sqrt| 'deriv) (make1dfun '(|Times| 1/2 (|Power| % -1/2)))) ;; not likely to be there
    ;;... etc rest of Trig functions

    (setf (get '|Sinh| 'deriv) (make1dfun '(|Cosh| %)))
    ;;... etc rest of Hyperbolic functions

    (setf (get '|ArcSin| 'deriv) (make1dfun '(|Power| (|Plus| -1 (|Power| % 2)) -1/2)))
    (setf (get '|ArcCos| 'deriv) (make1dfun '(|Times| -1 (|Power| (|Plus| 1 (|Times| -1 (|Power| % 2))))
					      -1/2)))
    (setf (get '|ArcTan| 'deriv) (make1dfun '(|Power| (|Plus| 1 (|Power| % 2)) -1)))
    (setf (get '|ArcSec| 'deriv) (make1dfun '(|Times| (|Power| (|Plus| 1 (|Times| -1 (|Power| % -2)))
						       -1/2)
					      (|Power| % -2))))
    ;;... etc rest of ArcTrig functions

    (setf (get '|ArcSinh| 'deriv) (make1dfun '(|Power| (|Plus| 1 (|Power| % 2)) -1/2)))
    (setf (get '|ArcCosh| 'deriv)
	  (make1dfun '(|Times| (|Power| (|Times| (|Plus| -1 %)(|Power| (|Plus| 1 %) -1)) -1/2)
		       (|Power| (|Plus| 1 %) -1))))
    (setf (get '|ArcTanh| 'deriv) (make1dfun '(|Power| (|Plus| 1 (|Times| -1 (|Power| % 2))) -1)))
    (setf (get '|ArcSech| 'deriv)
	  (make1dfun '(|Times| -1 (|Power| % -1)
		       (|Power| (|Times| (|Plus| 1 (|Times| -1 %))
				 (|Power| (|Plus| 1 %) -1))
			-1/2)
		       (|Power| (|Plus| 1 %) -1))))



    ;; odds and ends: Erf, ExpIntegralEi, Abs

    (setf (get '|Erf| 'deriv) (make1dfun '(|Times| 2
					   (|Power|
					    (|Times| (|Exp| (|Power| % 2)) (|Power| |Pi| 1/2)) -1))))

    (setf (get '|ExpIntegralEi| 'deriv) (make1dfun '(|Times| (|Exp| %) (|Power| % -1))))

    ;; line below is perhaps a problem when x=0..
	      
    (setf (get '|Abs| 'deriv) (make1dfun '(|Times| (|Abs| %)(|Power| % -1))))

;;;; integration properties
    (setf (get '|Log| 'integ)  '(|Times| % (|Plus| -1 (|Log| %))))
    (setf (get '|Sin| 'integ)  '(|Times| -1 (|Cos| %)))
    (setf (get '|Cos| 'integ)  '(|Sin| %))
    (setf (get '|Tan| 'integ)  '(|Times| -1 (|Log| (|Cos| %))))
    (setf (get '|Sec| 'integ)  '(|Times| 2 (|ArcTanh|(|Tan| (|Times| 1/2 %)))))
    (setf (get '|Cot| 'integ)  '(|Times| -1 (|Log| (|Sin| %))))
;;; etc
    (setf (get '|ArcSin| 'integ)
	  '(|Plus| (|Times| % (|ArcSin| %)) 
	    (|Power| (|Plus| 1 (|Times| -1 (|Power| % 2))) 1/2)))
    (setf (get '|ArcCos| 'integ)
	  '(|Plus| (|Times| % (|ArcCos| %)) 
	    (|Times| -1 (|Power| (|Plus| 1 (|Times| -1 (|Power| % 2))) 1/2))))
    (setf (get '|ArcTan| 'integ)
	  '(|Plus| (|Times| % (|ArcTan|  %))(|Times| -1/2 (|Log| (|Plus| 1 (|Power| % 2))))))
;;; etc
    (setf (get '|ArcTanh| 'integ)
	  '(|Plus| (|Times| % (|ArcTanh| %))(|Times| 1/2 (|Log| (|Plus| -1 (|Power| % 2))))))

    (setf (get '|Exp| 'integ) '(|Exp| %))
    ;;
    ;;Here's some more odds and ends

    (setf (get '|ExpIntegralEi| 'integ) '(|Plus| (|Times| % (|ExpIntegralEi| %))
					  (|Times| -1 (|Exp| %))))


    ;;int[ erf(_X) ] := _X * erf(_X) + Pi^(-1/2) * exp(-_X^2)

    (setf (get '|Erf| 'integ) '(|Plus| (|Times| % (|Erf| %))
				(|Power|
				 (|Times| (|Exp| (|Power| % 2)) (|Power| |Pi| 1/2)) -1)))

    (setf (get '|Abs| 'integ) '(|Times| 1/2 % (|Abs| %)))


;;; above computes derivative wrt 1st and only argument..
;;; how can we store the derivative wrt nth argument?

    ;; here's a place to start.  let q=EllipticE[f,m]
    ;; D[q,x] is Sqrt[1-m*Sin[f]]^2]*D[f,x]
    ;;          + (1/(2m)*(EllipticE[f,m]-EllipticF[f,m)*D[f,m]

    ;; in the most common situation m is not m(x) but a constant, so the 2nd term 
    ;; disappears.

    (setf (get '|EllipticE| 'deriv) 
	  (list #'(lambda(%1 %2)`(|Power|(|Plus| 1 (|Times| -1 ,%2 (|Power| (|Sin| ,%1) 2))) 1/2))
		#'(lambda(%1 %2) `(|Times| 1/2 (|Plus| (|EllipticE| ,%1 ,%2)(|Times| -1 (|EllipticF| ,%1 ,%2)))(|Power| ,%2 -1)))))

    (setf (get '|EllipticE| 'arity) 2)

    ;; Can we use the same deal for functions of one argument??

    (setf (get '|Sinc| 'deriv) 
	  (list #'(lambda(%1)`(Times (Plus (Times ,%1 (Cos ,%1)) (Times -1 (Sin ,%1)))(Power ,%1 -2)))))

    (setf (get '|Sinc| 'arity) 10)   ; any number >1  doesn't matter..

    (setattribute '|Module| '|HoldAll|)


    ;; implementing numeric evaluation, we will try to get the
    ;; lisp global variable *numer* set so that it is fast to check,
    ;; rather than going through (meval '|Numer|) all over the place


    (setf (gethash '|Numer| funnyvars) #'(lambda(h) ;;(format t "~%Numer set to ~s" h)
					   (setf *numer* h)))

    (chash '|Numeric|)
    ;; this is the list of Numeric functions in Mathematica 7.0 
    (map nil #'(lambda(r)
		 (if (get r symtab) nil (chash r))
		 (setattribute r '|Numeric|))
	 '(|Abs| |AiryAi|
	   |AiryAiPrime| |AiryAiZero| |AiryBi| |AiryBiPrime| |AiryBiZero|
	   |AppellF1| |ArcCos| |ArcCosh| |ArcCot| |ArcCoth| |ArcCsc| |ArcCsch|
	   |ArcSec| |ArcSech| |ArcSin| |ArcSinh| |ArcTan| |ArcTanh| |Arg| 
	   |ArithmeticGeometricMean| |BellB| |BesselI| |BesselJ| |BesselJZero|
	   |BesselK| |BesselY| |BesselYZero| |Beta| |BetaRegularized| |Binomial|
	   |CatalanNumber| |Ceiling| |ChebyshevT| |ChebyshevU| |Clip|
	   |Conjugate| |Cos| |Cosh| |CoshIntegral| |CosIntegral| |Cot| |Coth|
	   |Csc| |Csch| |DedekindEta| | Divide| |EllipticE| |EllipticF|
	   |EllipticK| |EllipticNomeQ| |EllipticPi| |Erf| |Erfc| |Erfi| |Exp|
	   |ExpIntegralE| |ExpIntegralEi| |Factorial| |Factorial2| |Fibonacci|
	   |Floor| |FractionalPart| |FresnelC| |FresnelS| |Gamma|
	   |GammaRegularized| |GegenbauerC| |HankelH1| |HankelH2|
	   |HarmonicNumber| |HermiteH| |Hypergeometric0F1|
	   |Hypergeometric0F1Regularized| |Hypergeometric1F1| 
	   |Hypergeometric1F1Regularized| |Hypergeometric2F1|
	   |Hypergeometric2F1Regularized| |HypergeometricU| |Im| |IntegerPart|
	   |InverseBetaRegularized| |InverseEllipticNomeQ| |InverseErf|
	   |InverseErfc| |InverseGammaRegularized| |InverseJacobiCD|
	   |InverseJacobiCN| |InverseJacobiCS| |InverseJacobiDC|
	   |InverseJacobiDN| |InverseJacobiDS| |InverseJacobiNC|
	   |InverseJacobiND| |InverseJacobiNS| |InverseJacobiSC|
	   |InverseJacobiSD| |InverseJacobiSN| |JacobiAmplitude| |JacobiCD|
	   |JacobiCN| |JacobiCS| |JacobiDC| |JacobiDN| |JacobiDS| |JacobiNC|
	   |JacobiND| |JacobiNS| |JacobiP| |JacobiSC| |JacobiSD| |JacobiSN|
	   |JacobiZeta| |KelvinBei| |KelvinBer| |KelvinKei| |KelvinKer|
	   |KleinInvariantJ| |LaguerreL| |LerchPhi| |Log| |LogGamma|
	   |LogIntegral| |LucasL| |MathieuC| |MathieuCharacteristicA|
	   |MathieuCharacteristicB| |MathieuCharacteristicExponent|
	   |MathieuCPrime| |MathieuS| |MathieuSPrime| |Max| |Min| |Minus| |Mod|
	   |ModularLambda| |Multinomial| |NevilleThetaC| |NevilleThetaD|
	   |NevilleThetaN| |NevilleThetaS| |ParabolicCylinderD| |Plus|
	   |Pochhammer| |PolyGamma| |PolyLog| |Power| |Quotient|
	   |QuotientRemainder| |RamanujanTau| |RamanujanTauL|
	   |RamanujanTauTheta| |RamanujanTauZ| |Re| |Rescale|
	   |RiemannSiegelTheta| |RiemannSiegelZ| |Round| |Sec| |Sech|
	   |SiegelTheta| |Sign| |Sin| |Sinc| |Sinh| |SinhIntegral| |SinIntegral|
	   |SphericalBesselJ| |SphericalBesselY| |SphericalHankelH1|
	   |SphericalHankelH2| |SphericalHarmonicY| |SpheroidalEigenvalue|
	   |SpheroidalJoiningFactor| |SpheroidalPS| |SpheroidalPSPrime|
	   |SpheroidalQS| |SpheroidalQSPrime| |SpheroidalRadialFactor|
	   |SpheroidalS1| |SpheroidalS1Prime| |SpheroidalS2| |SpheroidalS2Prime|
	   |Sqrt| |StruveH| |StruveL| |Subfactorial| |Subtract| |Tan| |Tanh|
	   |Times| |UnitStep| |WhittakerM| |WhittakerW| |Zeta| |ZetaZero|)) ;;whew

    (setf *initialized-mma* t)




    )

  )

;; All system-level $-stuff can be initialized and stored this way
(defun globinit(name val)
  (chash name); just in case it isn't already there
  (setf (gethash name (gethash name symtab)) val))

;; simple debugging tool: examine contents of symtable entry
(defun showhash(x)
  (maphash #'(lambda(key val) (format t "~%key=~s, val=~s" key val)) x))


; |Attributes| that evaluation routines use. maybe
;    - Flat [associative, flatten out nested expressions]
;    - Orderless [commutative, put args in order]
;    - |Hold|First [don't evaluate first arg yet]
;    - |Hold|Rest [only evaluate first arg now]
;    - |HoldAll| [don't evaluate any args now]
;    - Procedure [procedure to call to do actual evaluation]
;    - Default [default value for optional variables]

;;[version 3]?

;; the attribute semantics are probably OK as long as only global properties
;; are being recorded.
;; define ClearAttribute similarly
;; mma does not allow setting of |Attributes| other than
;;Protected, ReadProtected, Locked, Temporary, |Hold|First, |Hold|Rest, |HoldAll|, Flat, Orderless, OneIdentity, |List|able, Constant, Stub, N|Hold|First, N|Hold|Rest, N|HoldAll|, NumericFunction, |Sequence||Hold|, and |HoldAll|Complete.  [version 7]

;;bunch of additions, 1/2011 RJF

(defun setattribute(h att &optional (val '|True|)) ;; not in mathematica
  (if (typep h 'hash-table) nil (setq h (gethash h symtab)));; now its a ht
  (setf (gethash att h) val) ;; in h's hashtable, set the attribute att's value to true
  (setf(gethash '|Attributes| h);; also in h's hashtable, set its property of |Attributes| to a list
    (adjoin att (gethash '|Attributes| h)))
  )
(defun |Default|(h)
 (gethash '|Default| (gethash h symtab emptyht) '(|Sequence|))) 
  

(defun |SetAttributes|(hi attlist)  ;; this is in Mathematica.
  (let ((h (gethash hi symtab))) ;; get the hashtable for the symbol h.
  (cond ((atom attlist) ;; just one
	 (setattribute h attlist))
	(t (map nil #'(lambda(r)(setattribute h r))(cdr attlist))
	    (cons '|List| (gethash '|Attributes| h))
	    ;attlist
	    ))
  (|Attributes| hi)))

(defun |Attributes|(h)
  (cond ((constantp h) '(|List| |Constant|)) ; 3, 1/2, etc
	((setf h (gethash h symtab)) (ucons '|List| (gethash '|Attributes| h)))
	(t '(|List|)))) ;no attributes for a non-symbol


;; NO, BAD THINGS HAPPEN (setattribute '|Attributes| '|HoldAll|)

;;(chash 'ltrace) ;; lisp trace
;;(chash 'luntrace);; lisp untrace
;;(setattribute 'ltrace '|HoldAll|) 
;;(setattribute 'luntrace '|HoldAll|) 
;;(defun ltrace(&rest funs) (eval `(trace ,@funs)))
;;(defun luntrace(&rest funs)(eval `(untrace ,@funs)))


;; convert all real numbers to exact rational numbers

(defun |Real|(a b)  (+ a b))

;; this works only for integer x,  x>0 
(defun decimalsin(x)
  (ceiling (* (integer-length x) 0.30102999566398114d0)))


;; handle %, %%, etc.

(defun |Out| (&rest n)
 
  (gethash (ulist
	    (cond ((null n) (1- COUNT))	; just %, previous item
		 ((and (integerp (setf n (car n)))(minusp n)) ;%%, %%% etc
		  (+ COUNT n))
		 (t n)) ;; Out[random stuff]
	    )
	   (gethash '|Out| symtab)))

	 
(defun |Simp|(x)(simp x)) ;; rational simplification

(defun |Rat|(x)(into-rat x)) ;; leave the answer in rational form.
(defun |UnRat|(x)(outof-rat x)) ;; convert the answer to list form.

;; convert u to a rational with a single polynomial numerator
;; and denominator.  That is (x+1)^2 will be multiplied out.
;; result in rational form.


(defun |RatExpand|(u)
  (let* ((*expand* t) ;; global flag to rat program
	 (x (into-rat u)))
    (make-rat :numerator (make-fpe (fpe-expand (rat-numerator x))1)
	       :denominator
	       (make-fpe (fpe-expand (rat-denominator x)) 1))))

#+ignore (defun UnivariateDistinctDegreeFactorization (u)
  (let((x (into-rat u)))
    (make-rat :numerator   (poly-uddf  (rat-numerator x))
	      :denominator (poly-uddf  (rat-denominator x)))))
#+ignore (defun UnivariateSquareFree  etc)
#+ignore (defun UnivariateFactorizationMod(u p)
	   etc)

      

;; pick out parts of an expression.  x[[y]] parses to Part[x,y].
;; (a+r+b^c) [[3,2]] is c.
;;Generalizes somewhat in that (a+b^c)[[2,r]] returns (b^c)[[r]].
;; Does not handle negative part-numbers or lists of parts as
;; done in Mathematica. Also, won't decompose defstruct items
;; unless we do something about it for each structure...

(defun |Part|(u &rest k)(part1 (meval u) k)) ;; meval??

(defun part1 (u kl)(cond ((null kl) u)
			 ((and(integerp (car kl))
			      (>= (length u)(car kl)))
			  (part1 (nth (car kl) u)(cdr kl)))
			 ;; leave unevaluated part if can't handle it.
			 (t (ucons 'Part (ucons u kl)))))

;;If is HoldRest, so (car stuff) is evaluated.
(defun |If| (&rest stuff)
;  (format t "~%If stuff =~s, env=~s" stuff env)
  (cond ((eql (car stuff)  '|True|)	; test is True?? do we need to meval??
	 ;;(format t "~% then ~s mevals to ~s  with env ~s" (cadr stuff)(meval (cadr stuff)) env)
	 (meval (cadr stuff))) ;; evaluate the "then clause"
	((null (car stuff))		;test is False
	 ; evaluate the "else" if present else return Null
	 (if (caddr stuff)(meval (caddr stuff)) '|Null|)) 
	(t ;; test is neither true nor false,
	 (ucons '|If| stuff))))


	
  
;; basic simplification of |Times|

(defun |Times| (&rest x &aux (nums 1) oths)
 (dolist (h x   ;; iterate with h running over all args
	   ;;resultform
	   (cond((= 1 nums)
		 (if (null oths) 1 
		   (if (cdr oths) ;; more than one item in product 10/13/94
		       (ucons '|Times| (uniq(nreverse oths)))
		     (car oths))))
		((= nums 0) 0)
		((null oths) nums)
		(t (ucons '|Times| (ucons nums (uniq(nreverse oths)))))))
	 ;; body
	 (cond ((numberp h)(setq nums (* nums h))) ;; collect CL numbers
	       ;; if you find a rat, break out !
	       ((typep h 'rat)(return-from |Times|
					   (reduce-rat #'rat* 
						       (into-rat (car x))
						       (cdr x))))
	       (t (push h oths)))))

(defun |Condition|(a test)
  (let ((bool (meval test)))
  (cond ((or (null bool)(eql bool '|False|)) '|Null|)
	((eql bool '|True|) (meval a);a
			    )
				(t (ulist '|Condition| a test)))))
;; f[3, 4] /. (f[x_, y_] /; x > y -> gg)
;; f[5, 4] /. (f[x_, y_] /; x > y -> gg)


;; this definition allows  a=c; to take effect and return Null, so no display

(defun |CompoundExpression|(&rest x &aux result)
  
  (do* ((i x (cdr i))
	(j (car i)(car i)))
      ((null i) result)  ;; evaluate each element in turn, return last one.
    (setf result(meval j)))
 #+ignore  (catch :ret ;; if a return is executed somewhere inside here, 
	     )  
 ;; no, we want not to exit NOT from the Compound expression, but the
 ;; construction outside it
 )

(defun |Return|(x)
  ;;(format t "~% throwing :ret with value ~s" x)
  ;;(spopframe env) ;;hm. careful here.
       (throw :ret x))

(defun |Plus| (&rest x &aux (nums 0) oths)
  (dolist (h x ;; iterate with h running over all args
	    ;;resultform
	     (cond((zerop nums)
		   (cond ((null oths) 0)
			 ((null (cdr oths)) (car oths))
			 (t (ucons '|Plus| (uniq(nreverse oths))))))
		  ((null oths) nums)
		  
		  (t (ucons '|Plus| (ucons nums (uniq(nreverse oths)))))))
    ;; body
    (cond ((numberp h)(incf nums h))
	  ;; if a rat form, break out!
	  ((typep h 'rat)(return-from |Plus|
			   (reduce-rat #'rat+ (into-rat (car x)) (cdr x))))
	  (t (push h oths)))))


(defun powersimp (b &optional (e 1)) ;; need to handle (|Power| x)  --> x
  ( cond;;((and (integerp b)(integerp e)) 	(expt b e)) ;covered by next clause
   
    ((and (integerp b)(rationalp e)) (nthroot b e))
    ((and (rationalp b)(rationalp e))
     (|Times| (nthroot (numerator b) e)(nthroot (denominator b) (- e))))
    ;; maybe put in Complex for sqrt negative..
    ((and *numer* (numberp b)(numberp e)) (expt b e))
    ((and (eql '|Rat|(|Head| b))
	     (integerp e))
	(into-rat (uniq `(|Power| ,b ,e))))
       
       ((eql e 1) b)
       ((and (integerp e)(eql (|Head| b) '|Power|))  ;;(x^y)^2 -> x^(2*y). (x^2)^(1/2) no change.
	(ulist '|Power| (cadr b)(|Times| (caddr b) e)))
       
       (t (ulist '|Power| b e))))

;; this illustrates how to link lisp programs like powersimp into the
;; structure of rules defined by [x_ ...]:= ..
(defparameter powerrules  '(((|Power| (|Pattern| z(|BlankSequence|))) (lispapply powersimp  z  ))))
;; works
;;(eval-when '(load) 
;;  (setf (gethash '|SetDelayed| (gethash '|Power| symtab)) powerrules))
;;this makes powersimp the built-in default simplification program, but one can add additional
;; rules on top of it, e.g.   zsq^u_Rational :=z^(2*u)

;;until we convert everything to this scheme, we still need
(defun |Power| (b &optional (e 1)) (powersimp b e))

#+ignore;; leave Complex of symbols alone?
(defun |Complex|(re im)
  ;; insufficient for bigfloats or other number types that are not lisp numbers
  (cond ((numberp im)(cond ((numberp re)(complex re im)) ; both re and im are numbers
			   (t (uniq `(|Plus| ,re ,(complex 0 im))))))
	(t (uniq `(Plus ,re (|Times| ,im ,(complex 0 1)))))))

(defun |Complex|(re im)
  ;; insufficient for bigfloats or other number types that are not lisp numbers
  (cond ((and (numberp im)(numberp re))(complex re im)) ; both re and im are numbers
	(t `(|Complex| ,re ,im))))


(defun |Rational|(n d)
  ;; insufficient for bigfloats or other number types that are not lisp integers
  (cond ((integerp d)(cond ((integerp n)(/ n d)) ; both n and d are integers
			   (t (uniq `(|Times| , n ,(/ 1 d))))))
	(t ;;(uniq `(|Times| ,n (|Power| ,d  -1)))
	  `(|Rational| ,n ,d))))

#+ignore
(defun rationalsimp(n d)
  ;; insufficient for bigfloats or other number types that are not lisp integers
  (cond ((and(integerp d)(integerp n))(/ n d))
	(t  (uniq `(|Times| ,n (|Power| ,d  -1))))));; probably want to just leave (Rational n d)

#+ignore
(defparameter rationalrules  '(((|Rational| (|Pattern| %n(|Blank|))
					    (|Pattern| %d(|Blank|)))
					     (lispapply rationalsimp  %n %d  )))) ;;nope

;;(setf (gethash '|Rational| (gethash '|Rational| symtab)) rationalrules)
;; note: Timing is a |HoldAll| function... otherwise the evaluation
;; of the argument would come first, and the timing would
;; be of an already evaluated expression.

(defun |Timing|(x) ; x is an expression, perhaps compound, uneval'd
  
  (let*((timeunit (/ 1.0 internal-time-units-per-second))
	(timesofar (get-internal-run-time))
	(result (meval x)))
 ;; (format t "~% result =~s" result)
    (uniq `(|List| (|Times| , (*(- (get-internal-run-time) timesofar)
		      timeunit)
		    Second) ,result))))

;; if we are stuck with all one case
;; then we are forced to do something like this for
;; every function that is already defined in lisp with the
;; same name as in mathematica (tm)


;; more notes
;; Sin[x_]/; x>3-> S[x] 
;; parses to
;; (Rule (Condition (Sin (Pattern x (Blank))) (Comparison x Greater 3)) (S x))

;; we don't have evaluation of Block  implemented.
;; 

;; probably should add evaluation of functions and slots. 
;; eg  #+1&[4]  should return 5.
#|
(break "t")

;;(ww (Pattern x (Blank))) Condition aha
;;(Comparison x Greater 5)
;;  w[x_]:= aha /; x>5    parses into something like ...
;;((  (ww (Pattern x (Blank)))    . (Condition aha  (Comparison x Greater 5))   ))

;;but we need (Rule (Condition (ww (Pattern x (Blank))) (Comparison x Greater 5)) aha)
;;or something close to that

;;(trial '(Condition (ww (Pattern x (Blank))) (Comparison x Greater 5)) '(w 10))
;;(trial '(Condition (ww (Pattern x (Blank))) (Comparison x Greater 5)) '(w 4))

|#

;;just a hack ..
#|
(defun macint (x y)
   (max2mma  (maxima::$integrate   (mma2max x)  (mma2max y))))

|#

(defun |Every| (exp iter)
  (every1 exp iter))

(defun |Some| (exp iter)
  (some1 exp iter))



(defun every1 (exp iter)  ;; kick out if anything is False (=nil)
  ;;Exp is an expression with a free variable itvar
  ;;iter looks like {i,low,hi}  or (|List| i low hi) in Lisp
  (case (length iter)
    (1 (error "invalid iterator ~s" iter))
    (2 ;; (List count)
     (let ((count (second iter)))
       (cond((not(integerp count)) (format t "~%expected integer iterator ~s" iter)
	     (signal 'error))
	    ((< count 0)(format t "~%expected non-negative iterator ~s" iter)(signal 'error))
	    (t (setf exp (meval exp))
	       (loop for i from 1 to count do (if (eql exp '|True|) nil
						(return nil)))
	       '|True|))))
    (3 
     ;; (List i hi);  no low, assumed 1
     ;; or (list i (List a b c ...))
     (let ((tt (third iter))
	   (itvar (second iter)))
       (cond((and(integerp tt ) (>= tt 1))
	     (every1 exp (uniq `(|List| ,(second iter) 1 ,tt)))) ;; just count from 1.
	    ((and (consp tt)(eql (car tt) '|List|))
	     (spush env iter nil)
	     (do ((i (cdr tt) (cdr i))
		  (res nil 
		       (progn (schange env itvar (car i))
			      (if (eql '|True| (meval exp) ) nil (let()(spop env)(return nil))))))
		 ((null i) (spop env) '|True|)))))) ;; kept on going until exhausted so must be
    ((4 5) 
     (let ((itvar (second iter))	     ;; (List i low hi [step])
	   (hi  (meval (fourth iter)))	     ;hi
	   (step (or (meval (fifth iter)) 1))) ;if missing, then 1
       (spush env itvar 0)		       ; reserve a space
       ;; the case of {i, 1, 10}   or {i,1,10,2} ;; set step
       (do ((i (meval (third iter)) (+ step i))
	    (res nil
		 (progn (schange env itvar i)
			 (if (eql '|True| (meval exp) ) nil (let()(spop env)(return nil))))))
	   ((> i hi) (spop env) '|True|))))
    ))

(defun some1 (exp iter)  ;; kick out if anything is True
  ;;Exp is an expression with a free variable itvar
  ;;iter looks like {i,low,hi}  or (|List| i low hi) in Lisp
  (case (length iter)
    (1 (error "invalid iterator ~s" iter))
    (2 ;; (List count)
     (let ((count (second iter)))
       (cond((not(integerp count)) (format t "~%expected integer iterator ~s" iter)
	     (signal 'error))
	    ((< count 0)(format t "~%expected non-negative iterator ~s" iter)(signal 'error))
	    (t (setf exp (meval exp))
	       (loop for i from 1 to count do (if (eql exp '|True|) 
						(return nil) nil))
	       '|True|))))
    (3 
     ;; (List i hi);  no low, assumed 1
     ;; or (list i (List a b c ...))
     (let ((tt (third iter))
	   (itvar (second iter)))
       (cond((and(integerp tt ) (>= tt 1))
	     (some1 exp (uniq `(|List| ,(second iter) 1 ,tt)))) ;; just count from 1.
	    ((and (consp tt)(eql (car tt) '|List|))
	     (spush env iter nil)
	     (do ((i (cdr tt) (cdr i))
		  (res nil 
		       (progn (schange env itvar (car i))
			      (if (eql '|True| (meval exp)) (let()(spop env)(return '|True|)) nil))))
		 ((null i) (spop env) nil)))))) ;; kept on going until exhausted so none..
    ((4 5) 
     (let ((itvar (second iter))	     ;; (List i low hi [step])
	   (hi  (meval (fourth iter)))	     ;hi
	   (step (or (meval (fifth iter)) 1))) ;if missing, then 1
       (spush env itvar 0)		       ; reserve a space
       ;; the case of {i, 1, 10}   or {i,1,10,2} ;; set step
       (do ((i (meval (third iter)) (+ step i))
	    (res nil
		 (progn (schange env itvar i)
			 (if (eql '|True| (meval exp) )  (let()(spop env)(return nil)) nil))))
	   ((> i hi) (spop env) nil))))
    ))


#|

;; This is the way to make rules / function definitions in LISP.
;; we can make these rules for, for example, Power.  Then override them
;; by user stuff as necessary.

(setf h5rule '((h5 (Pattern z(BlankSequence))) (lispapply h5fun  z  )))) ;; works
;;(setf h5rule '((h5 (Pattern z(BlankSequence))) (lispapply (lambda(&rest r)(print r))  z  )))) ;; works
(defun h5fun(&rest r)(cons 'defaulth5 r))

(setf h5rules (list h5rule))
(setf (gethash '|SetDelayed| (gethash 'h5 symtab)) h5rules)
(chash 'lispapply)
(setattribute 'lispapply '|HoldAll|)


|#


(defun iroot (a n)   
  ;; computes a^(1/n)  integer a>0 n>0 see Fitch, SIGSAM Bull Nov 74
  ;; second value is distance from exact root
  (let ((ila (integer-length a)))
   
    (cond ((= a 0)(values 0 0))
	  ((< ila n) (values 1 (1- a)))
	  (t				;assumes integer a>0 n>=2
	   (do ((x (expt 2 (1+ (truncate ila n)))
		   (- x (truncate (+ n1 bk) n)))
		(n1 (1- n)) (xn) (bk))
	       (nil)
	     (cond ((<= (setq bk (- x (truncate a (setq xn (expt x n1))))) 0)
		    (return (values x (- a (* x xn)))))))))))

(defun nthroot(a n) ;; eg.  (nthroot 8 2/3);  (nthroot 9 1/3)
  (if (< a 0)(ulist '|Power| a n)
  (multiple-value-bind
      (r leftover)
      (iroot a (denominator n))
    (if (= leftover 0)(expt r (numerator n)) (ulist '|Power| a n)))))


(defun  sqrtexact(a)  ;; a is an integer. return sqrt(a)
  (let ((complex nil)(b a))
    (cond ((< b 0)(setf complex t b (- a))))
    (let ((rt (nthroot b 1/2)))
      (cond ((integerp rt) (if complex (setf rt (complex 0 rt))))
	    (t (setf rt (ulist '|Power| a 1/2))))
      rt)))

;; setting LineLength=100 will change COL

(setf (gethash 'LineLength funnyvars)#'(lambda(r)(declare (special COL))
					      (and (integerp r)(> r 10)(setf COL r))))

