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

(promise:then p1 
              (lambda (x) (print (list 'accepted x)))
              (lambda (x) (print (list 'rejected x)))))

(promise:_catch
    (promise:then-resolve p1 (lambda (x) (print (list 'accepted x))))
    (lambda (x) (print (list 'rejected x))))


(promise:then-resolve (promise:all (promise:resolve 1) (promise:resolve 2)))



```

## Copyright
Copyright © 2017 Vladimir Mezentsev

## License
GNU General Public License v3.0


[jscl]: <https://github.com/jscl-project/jscl>
[moren]: <https://github.com/vlad-km/moren-electron>

