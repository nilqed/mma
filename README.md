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







[^1]: where `~` has to be adjusted accordingly if Quicklisp is installed elsewhere.
