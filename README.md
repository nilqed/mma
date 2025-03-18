# mma
Mock-Mma by Richard J. Fateman

This is a version that can be loaded using [Quicklisp](https://www.quicklisp.org/beta/)
when this repo is cloned into the `~/quicklisp/local-projects/` folder[^1].


## Installation

    $ cd :~/quicklisp/local-projects/
    $ git clone https://github.com/nilqed/mma.git

## Running
Start your *lisp* (tested with ABCL, ECL, SBCL so far): 

    (ql::quickload :cl-package-locks)
    (cl-package-locks::unlock-package :common-lisp)
    (ql::quickload :mma)
    (cl-package-locks::lock-package :common-lisp)
    (in-package :mma)
    (tl)

For convenience there is a file `run-mma.lisp` in the `mma` folder that
does exactly as above when

     your-lisp --load "path-to/run-mma"

*Note* that it is assumed that `quicklisp` is available in your Lisp, otherwise you have
to issue a

    (load "~/quicklisp/setup")

before the other commands (e.g. see `test-abcl.txt`).

## Example


        kfp@omega:~/quicklisp/local-projects/mma$ sbcl --load "run-mma"
        This is SBCL 2.2.9.debian, an implementation of ANSI Common Lisp.
        More information about SBCL is available at <http://www.sbcl.org/>.
        
        SBCL is free software, provided as is, with absolutely no warranty.
        It is mostly in the public domain; some portions are provided under
        BSD-style licenses.  See the CREDITS and COPYING files in the
        distribution for more information.
        To load "cl-package-locks":
          Load 1 ASDF system:
            cl-package-locks
        ; Loading "cl-package-locks"
        
        To load "mma":
          Load 1 ASDF system:
            mma
        ; Loading "mma"
        
        Mock-Mma  14: 9 Tuesday, Mar 18, 2025
        
        In[1] := D[x^p*Sin[x],x]
        
                  p
                 x  (p Sin[x] + x Cos[x])
        Out[1] = ------------------------
                            x
        In[2] := Quit[]
        
        Exited to Lisp
        * (quit)
        kfp@omega:~/quicklisp/local-projects/mma$kfp@omega:~/quicklisp/local-projects/mma$
        
See also `test-abcl.txt` and `test-ecl.txt`. Other CL's should work as well. Using `npt`
showed a problem with loading `ASDF 3.0+` which is unrelated with `mma`, though.


### Changelog




[^1]: where `~` has to be adjusted accordingly if Quicklisp is installed elsewhere.
