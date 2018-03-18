# Promise  

Package **Promise** - lisp ([JSCL][jscl]) wrap for JavaScript Promise.

It is intended for use in the [Moren][moren] environment.

## Status

`Development`

## Compilation

```lisp
   (lores:qload :promise :storejs t)
   (lores:modlink :promise "prom.js")
```

## Use

```lisp
(setq p1 (promise:make
            (lambda (res rej)
                 (#j:setTimeout
                     (lambda () (case (random 2)
                                  (0 (funcall rej "no"))
                                  (1 (funcall res "yes")))) 100)))
(promise:is-promise p1)
;;=> t

(promise:then p1 
              (lambda (x) (print (list 'accepted x)))
              (lambda (x) (print (list 'rejected x)))))

(promise:_catch
    (promise:then-resolve p1 (lambda (x) (print (list 'accepted x))))
    (lambda (x) (print (list 'rejected x))))

(promise:then-resolve (promise:all (promise:resolve 1) (promise:resolve 2)))
```

## Promisify

For example we use [pify][pify]. 

```lisp                         
                                                             
(setf #j:pify (require "pify"))

(defun somefn (arg1 arg2 callback &rest other)
    ;; some code
    (if flag 
       (funcall callback nil arg1) ;; its (resolve arg1)
       (funcall callback arg2 nil)) ;; its (reject arg2)
))

(promise:then (funcall (#j:pify #'somefn) 123 456)
              (lambda (x) (print x))  ;; => resolve 123
              (lambda (x) (print x))) ;; => rejected 456

```

## Copyright
Copyright © 2017,2018 Vladimir Mezentsev

## License
GNU General Public License v3.0


[jscl]: <https://github.com/jscl-project/jscl>
[moren]: <https://github.com/vlad-km/moren-electron>
[pify]: <https://github.com/sindresorhus/pify>
