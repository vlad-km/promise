;;; -*- mode:lisp; coding:utf-8  -*-

;;; Lisp wrapper for JavaScript Promise
;;; This file is part of the MOREN Environment
;;; Copyright © 2017 Vladimir Mezentsev
;;;


;;;
;;; (setq prom (#j:make_Instance #j:window "Promise" fn))
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
    (unless #j:make_Instance
        (let ((make-instance-proto
                "var make_Instance  =  function () {
                     var args = [].concat(null,Array.prototype.slice.call(arguments,2));
                     var fn = arguments[0][arguments[1]];
                     return new (Function.prototype.bind.apply(fn,args))();
                    };"))
            (#j:eval make-instance-proto))))


(eval-when (:compile-toplevel :load-toplevel :execute)
    (unless (find-package :promise)
        (make-package :promise :use (list 'cl)))
    )


(in-package :promise)
(export '(jscl::new jscl::oget jscl::concat jscl::list-to-vector))

(in-package :cl-user)


;;; EOF
