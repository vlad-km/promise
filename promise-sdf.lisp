;;; -*- mode:lisp; coding:utf-8  -*-

(lores:defsys :promise
    :path  "git/promise"
    :components ((:file "package")
                 (:file "promise" :depends ("package"))))


;;; EOF
