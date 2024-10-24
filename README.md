# FOR using LOOP

Easy `FOR` macro that expands to `LOOP`.

Zero dependencies, extremely lightweight, directly expands to `LOOP`,
easily nesting, no extra burden.

## Examples

```common-lisp
(for (i range 10)
     (prev previous i 0)
     (collect (list prev i)))

;; Easy nested loop
(for* (present-symbol sym in-package "CL")
      (key val in-plist (symbol-plist sym))
      (print key)) 
```

## Clauses

_*All suitable verb clauses can be wrote in -ing form_

_*We don't provide a variable clause (WITH clause), as using let surrounds will be more clear for reading._

### Range clause

	(var {range | ranging} n) => loop for var from 0 to (1- n)
	(i fixnum range 10) => loop for i of-type fixnum from 0 to (1- 10)
	(i range 0 10) => loop for i from 0 to 10
	(i range 0 10 [by] 2) loop for i from 0 to 10 by 2

Specially:

	(i index) | (i range) => loop for i from 0

### From... To...

	(i fixnum to 10) => loop for i of-type fixnum from 0 to 10
	(i from 0 to 10 by 2) => loop for i from 0 to 10 by 2

### Equal... Then...

	(i 10 then (1+ i)) => loop for i = 10 then (1+ i)
	(i = (random)) => loop for i = (random)
	(i fixnum = 10 then (1+ i)) => loop for i of-type fixnum = 10 then (1+ i)

### In / On / Across

	(i fixnum in list [by func]) => loop for i of-type fixnum in list by func
	((i j . k) on list [by func]) => loop for (i j . k) on list by func
	(c character across "string") => loop for c of-type character across "string"

### Plist / Alist

	(key val in-plist '(:a 1 :b 2)) => loop for (key val) on '(:a 1 :b 2) by #'cddr
	(key nil in-alist '((:a . 1) (:b . 2))) => loop for (key . val) in '((:a . 1) (:b . 2))

### Hash Table

	(key {hash-key | hash-keys | in-hash-key | in-hash-keys | table-keys} table) => loop for key being each hash-key of table
	(val {hash-value | hash-values | in-hash-value | in-hash-values | table-values}) loop for val being each hash-value of table
	(key val {table | hashtable | table-pairs | hash-table | in-table | in-hashtable | in-hash-table} table) => loop for key being each hash-key of table for val = (gethash key table)

### Package Symbols

	(present-symbol sym in-package *package*) => loop for sym being each present-symbol of *package*

### Accumulation / Termination / Name

	(named sym) => loop named 10
	(thereis i) => thereis i
	(for (i :range 10) (when i (collect i r))) => loop for i from 0 to (1- 10) when i collect i into r

For extra, we also support the `multiply` clause, similar with `iterate`:

	(for (i :range 1 10) (:multiply i)) => (loop :with #:accumulator45 := 1 :for i :from 1 :to 10 :do (setq #:accumulator45 (* #:accumulator45 i)) :finally (return #:accumulator45))

Just like in the `LOOP`, they can only appear in parsed form, The following is invalid:

	(for (repeat 10) (let ((i (random))) (collect i))) => Undefined operator COLLECT

### Finders

Similar with `iterate`, but can only appear in parsed form (not everywhere).

- **find | finding** *expr* *test* : Evaluate *test*, or funcall it with *expr* if it's a function, and immediatly return expr when the result is non-`nil`.

- **find | finding** *expr* **max | maximize** *test* *\[peak-value | (peak-value peak-num)]*:
Evaluate *test*, or funcall it with *expr* if it's a function, bind the maximum result to *peak-num*, and correspond value of *expr* to *peak-value*. returns `(values peak-value peak-num)`, if there's no other `finally` clause.

	(find (- i) max (mod i 5)) => (loop :with #:peak-num08 :and #:peak-value09 :and #:value10 :do (setq #:value10 (mod i 5)) :when (or (null #:peak-num08) (> #:value10 #:peak-num08)) :do (setq #:peak-num08 #:value10 #:peak-value09 (- i)) :end :finally (return (values #:peak-value09 #:peak-num08)))

### Initially / Finally

	(init (incf a) (setf b 0)) => loop initially (progn (incf a) (setf b nil))
	(final (incf a) (setf b 0) => finally (return (progn (incf a) (setf b 0))))

### Conditions

They're processed recursively:

	(when i (when (evenp i) (collect i) (sum i r)) (print i)) => loop when i when (evenp i) collect i and sum i into r end end do (print foo)

### Do Clause

All other forms are treated as `DO` clauses.

	(for (i :range) (print i) (print (1+ i)) (until (= i 10))) => loop for i from 0 do (print i) (print (1+ i)) until (= i 10)

## Extending Clauses

The function `define-for-extend-clause` provides a simple interface to
extend `FOR` and `FOR*`. See its document for details.

By the way, since this macro is very simple, you can easily redefining
the macros and core function (`for-collect-clauses`) with your own
codes, even totally reimplement it in your own.

## Motivation

I've heavily used `LOOP` for years, and finally unbearable.

I think `LOOP` is the best Lisp macro on logic, but an (anyway)
failure on its syntax. Especially the `do` clause, it always burden me
with three more indentation and rapidly break out my fill columns.

So I tried different. One is the famous `iterate`. Iterate is very
nice, but I still need to write a lot of `FOR` for variable drivers.
Why I need to write so many `FOR`s?

Then I tried Shinmera's `For`. It's way more better, especially the
`range` clause it provided, really saved me from a lot of `FROM ... TO
...`.  But sometimes its performance is not very ideal...

```common-lisp
(let* ((list (alexandria:iota 1000000))
       (for (lambda ()
              (for-minimal:for ((i :in list) (result :collect i))) nil))
       (iter (lambda ()
               (iterate:iter (iterate:for i :in list) (iterate:collect i)) nil))
       (loop (lambda ()
               (loop for i :in list :collect i) nil)))
  (time (funcall for))
  (time (funcall iter))
  (time (funcall loop)))
```

The result (on LispWorks x64):

	Timing the evaluation of (FUNCALL FOR)
	...
	Elapsed time =        5.620
	
	Timing the evaluation of (FUNCALL ITER)
	...
	Elapsed time =        1.625
	
	Timing the evaluation of (FUNCALL LOOP)
	...
	Elapsed time =        1.292

Although the result is highly implementation-dependent, and the
compilation can gives much lower distinctions, I still cannot fully
trust the 3rd party libraries. Yeah, `LOOP` is fairly powerful enough,
and has the foremost support from implementations, there's only
something unbearable in syntax. So why not makes a simple macro that
expands to `LOOP`? So that we can get benefit from both syntax and
supportment. That's why I try to do this work.

I implemented the first version of `FOR` using the `TRIVIA:MATCH`, but
soon I found that macroexpanding the `MATCH` form takes too much time.
I'm afraid if it's not appropriate even if the codes will only be
invoked during macro expansion, so I rewroted it using tree-shape
matching based on pure CL functions. It becomes much difficult to
read, but still acceptable for me, at least compared to a bunch of
`LOOP`s :P

## Acknowledgement

Thanks my headmate May who supports me.

Supporting Neurodivengent & LGBTQIA+ & Plurality!
