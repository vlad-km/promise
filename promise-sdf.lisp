;;; -*- mode:lisp; coding:utf-8  -*-

;;; Lisp wrapper for JavaScript Promise
;;; This file is part of the promise package
;;; Copyright © 2017,2018 Vladimir Mezentsev
;;;

(lores:defsys :promise
    :path  "git/promise"
    :components ((:file "package")
                 (:file "promise" :depends ("package"))))


;;; EOF
