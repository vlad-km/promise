;;; -*- mode:lisp; coding:utf-8  -*-

;;; Lisp wrapper for JavaScript Promise
;;; This file is part of the promise package
;;; Copyright © 2017,2018 Vladimir Mezentsev
;;;


(in-package :promise)
(export '(jscl::oget))

;;;
;;; MAKE
;;; Arguments
;;;
;;;     fn - promise resolver. Function with two arguments
;;;          -> (lambda (resolve &optional reject))
;;; => promise
(defun make (fn)
    (#j:make_Instance #j:window "Promise" fn))


;;; RESOLVE
;;; Arguments
;;;
;;;     val - Argument to be resolved by this Promise. Can also be a Promise
;;;           or a thenable to resolve
;;;
;;; => promise object that is resolved with the given value.
(defun resolve (val)
    (#j:Promise:resolve val))



;;; REJECT
;;; => promise object that is rejected with the given reason
(defun reject (val)
    (#j:Promise:reject val))


;;; ALL
;;; Arguments
;;;
;;;     promises - promise's list
;;;
;;; => single promise that resolves when all of the promises in the arguments have resolved
;;;    or when the iterable argument contains no promises.
;;;    It rejects with the reason of the first promise that rejects.
;;;
;;; (promise:all (promise:resolve 11) (promise:resolve "Ok"))
(defun all (&rest promises)
    (let ((ar (jscl::list-to-vector promises)))
        (#j:Promise:all ar)))

;;; RACE
;;; Arguments
;;;     promises - promise's list
;;;
;;; => pending promise that resolves or reject
(defun race (&rest promises)
    (let ((ar (jscl::list-to-vector promises)))
        (#j:Promise:race ar)))


;;; THEN
;;; Arguments
;;;     promise - promise
;;;     ok     - onResolve
;;;               => (lambda (reason) ...)
;;;     nok    - onReject
;;;              => (lambda (reason) ...)
;;; => promise
;;;
;;; (setq p1 (promise:make
;;;             (lambda (res rej)
;;;                 (#j:setTimeout
;;;                     (lambda () (case (random 2)
;;;                                  (0 (funcall rej "no"))
;;;                                  (1 (funcall res "yes")))) 100)))
;;; (promise:then p1 (lambda (x) (print (list 'accepted x)))
;;;                  (lambda (x) (print (list 'rejected x))))
;;;
(defun then (promise ok nok)
    (funcall ((oget promise "then" "bind") promise ok nok)))

(defun then-resolve (promise ok)
    (funcall ((oget promise "then" "bind") promise ok)))


;;; _CATCH
;;; Arguments
;;;     promise - promise
;;;     rej     - onRejected
;;;               => (lambda (reason) ...)
;;;
;;; => promise
;;;
;;; (promise:_catch
;;;     (promise:then-resolve p1 (lambda (x) (print (list 'accepted x))))
;;;     (lambda (x) (print (list 'rejected x))))
;;;
(defun _catch (promise fn)
    (funcall ((oget promise "catch" "bind") promise fn)))


;;; is promise predicate
;;;
;;; (promise:is-promise *somewhat)
;;; => t if *somewhat has type promise
;;;    nil in other cases
(defun is-promise (obj)
    (if (or (unless (jscl::objectp obj) t)
            (listp obj)
            (stringp obj)
            (arrayp obj))
        (return-from is-promise nil))
    (if (functionp (oget obj "then")) t nil))



(in-package :cl-user)
;;; EOF
