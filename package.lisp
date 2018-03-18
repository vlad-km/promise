;;; -*- mode:lisp; coding:utf-8  -*-

;;; Lisp wrapper for JavaScript Promise
;;; This file is part of the promise package
;;; Copyright © 2017,2018 Vladimir Mezentsev
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


(defpackage #:promise
  (:use #:cl)
  (:export #:make
           #:resolve #:reject
           #:all #:race
           #:then #:then-resolve
           #:_catch
           #:is-promise))


;;; EOF
