;; Copyright (c) 2024, April & May
;; SPDX-License-Identifier: 0BSD

(defpackage for-loop
  (:use :cl))
(in-package for-loop)

(defvar *for-extend-clauses-alist* nil
  "Alist for extend clause of `FOR'
each CAR is the predicate function, and CDR is the yield function.")

(defun define-for-extend-clause (predicate-function yield-function)
  "Define extend clause for `FOR-LOOP:FOR' and `FOR-LOOP:FOR*'.

The predicate and yield functions will be called with current clause
form (e.g. `(i from 1 to 10)'), The predicate function should return
true if the form is matched, then the yield function should return 5
values: The list of symbols to be added to the INITIALLY clause, WITH
clause, FOR clause, BODY clause, and FINALLY clause."
  (push (cons predicate-function yield-function) *for-extend-clauses-alist*))

(defun for-collect-clauses (clauses &optional andp)
  (let (init-clauses with-clauses prev-clauses for-clauses body-clauses final-clause
         last-do-p this-do-p)
    (flet ((same (s1 s2)
             (declare (inline same))
             (when (and (or (symbolp s1) (stringp s1))
                        (or (symbolp s2) (stringp s2)))
               (string-equal s1 s2))))
      (symbol-macrolet ((1st (first   clause))
                        (2nd (second  clause))
                        (3rd (third   clause))
                        (4th (fourth  clause))
                        (5th (fifth   clause))
                        (6th (sixth   clause))
                        (7th (seventh clause)))
        (macrolet ((clause (clause-length &rest sames)
                     (nconc `(and (= (length clause) ,clause-length))
                            (loop for (val1 val2) in sames collect `(same ,val1 ',val2))))
                   (add (pos syms)
                     `(setf ,pos (nconc ,pos (list (if (and andp ,pos (null this-do-p))
                                                       (cons :and ,syms)
                                                     ,syms))))))
          (prog ((clause (car clauses)))
            start-loop
            ;; extensions
            (loop for (pred . yield) in *for-extend-clauses-alist*
                  when (funcall pred clause)
                    do (multiple-value-bind (init with for body final)
                           (funcall yield clause)
                         (when init (add init-clauses init))
                         (when with (add with-clauses with))
                         (when for (add for-clauses for))
                         (when body (add body-clauses body))
                         (when final (setq final-clause final)))
                       (go next-loop))
            ;; index
            (when (clause 2 (2nd "INDEX"))
              (add for-clauses (list 1st :from 0))
              (go next-loop))
            ;; range
            (cond ((and (>= (length clause) 2) (same 2nd :range))
                   (cond ((clause 2)
                          (add for-clauses (list 1st :from 0)))
                         ((clause 4 (3rd by))
                          (add for-clauses (list 1st :from 0 :by 4th)))
                         ((clause 3)
                          (add for-clauses (list 1st :to `(1- ,3rd))))
                         ((clause 4)
                          (add for-clauses (list 1st :from 3rd :to 4th)))
                         ((clause 5 (4th "BY"))
                          (add for-clauses (list 1st :to `(1- ,3rd) :by 5th)))
                         ((clause 5)
                          (add for-clauses (list 1st :from 3rd :to 4th :by 5th)))
                         ((clause 6 (5th "BY"))
                          (add for-clauses (list 1st :from 3rd :to 4th :by 6th)))
                         (t (go from-to))))
                  ((and (>= (length clause) 3) (same 3rd :range))
                   (cond ((clause 3)
                          (add for-clauses (list 1st :of-type 2nd :from 0)))
                         ((clause 5 (4th "BY"))
                          (add for-clauses (list 1st :of-type 2nd :from 0 :by 5th)))
                         ((clause 4)
                          (add for-clauses (list 1st :of-type 2nd :to 3rd)))
                         ((clause 5)
                          (add for-clauses (list 1st :of-type 2nd :from 4th :to 5th)))
                         ((clause 6 (5th "BY"))
                          (add for-clauses (list 1st :of-type 2nd :from 4th :by 6th)))
                         ((clause 6)
                          (add for-clauses (list 1st :of-type 2nd :from 4th :to 5th :by 6th)))
                         ((clause 7 (6th "BY"))
                          (add for-clauses (list 1st :of-type 2nd :from 4th :to 5th :by 7th)))
                         (t (go from-to))))
                  (t (go from-to)))
            (go next-loop)
            from-to
            (when (and (>= (length clause) 3) (or (member 2nd '("FROM" "TO" "DOWNFROM" "UPFROM" "DOWNTO" "UPTO" "BY") :test #'same)))
              (add for-clauses clause)
              (go next-loop))
            (when (and (>= (length clause) 4) (or (member 3rd '("FROM" "TO" "DOWNFROM" "UPFROM" "DOWNTO" "UPTO" "BY") :test #'same)))
              (add for-clauses (nconc (list 1st :of-type 2nd) (cddr clause)))
              (go next-loop))
            lists
            (cond ((clause 4 (3rd "IN-PLIST"))
                   (add for-clauses (list (list 1st 2nd) :on 4th :by #+lispworks #'cddr #-lispworks 'cddr)))
                  ((clause 4 (3rd "IN-ALIST"))
                   (add for-clauses (list (cons 1st 2nd) :on 4th)))
                  (t (go tables)))
            (go next-loop)
            tables
            (when (>= (length clause) 3)
              (cond ((member 2nd '("HASH-KEY" "HASH-KEYS" "IN-HASH-KEY" "IN-HASH-KEYS" "TABLE-KEYS") :test #'same)
                     (add for-clauses (list 1st :being :each :hash-key :of 3rd)))
                    ((member 2nd '("HASH-VALUE" "HASH-VALUES" "IN-HASH-VALUE" "IN-HASH-VALUES" "TABLE-VALUES") :test #'same)
                     (add for-clauses (list 1st :being :each :hash-value :of 3rd)))
                    ((and (clause 4) (member 3rd '("TABLE" "HASHTABLE" "TABLE-PAIRS" "HASH-TABLE" "IN-TABLE" "IN-HASHTABLE" "IN-HASH-TABLE") :test #'same))
                     (add for-clauses (list 1st :being :each :hash-key :of 4th :using `(:hash-value ,2nd))))
                    (t (go in)))
              (go next-loop))
            in
            (cond ((member 2nd '("IN" "ON") :test #'same)
                   (let ((kw (if (same 2nd "IN") :in :on)))
                     (cond ((clause 5 (4th "BY"))
                            (add for-clauses (list 1st kw 3rd :by 5th)))
                           ((clause 4)
                            (add for-clauses (list 1st kw 3rd :by 4th)))
                           (t (add for-clauses (list 1st kw 3rd))))))
                  ((member 3rd '("IN" "ON") :test #'same)
                   (let ((kw (if (same 3rd "IN") :in :on)))
                     (cond ((clause 6 (5th "BY"))
                            (add for-clauses (list 1st :of-type 2nd kw 4th :by 6th)))
                           ((clause 5)
                            (add for-clauses (list 1st :of-type 2nd kw 4th :by 5th)))
                           (t (add for-clauses (list 1st :of-type 2nd kw 4th))))))
                  (t (go across)))
            (go next-loop)
            across
            (cond ((clause 3 (2nd "ACROSS"))
                   (add for-clauses (list 1st :across 3rd)))
                  ((clause 4 (3rd "ACROSS"))
                   (add for-clauses (list 1st :of-type 2nd :across 4th)))
                  (t (go equal)))
            (go next-loop)
            equal
            (cond ((clause 4 (3rd "THEN"))
                   (add for-clauses (list 1st := 2nd :then 4th)))
                  ((clause 5 (4th "THEN"))
                   (add for-clauses (list 1st :of-type 2nd := 3rd :then 5th)))
                  ((clause 3 (2nd "="))
                   (if (clause 5 (4th "THEN"))
                       (add for-clauses (list 1st := 3rd :then 5th))
                     (add for-clauses (list 1st := 3rd))))
                  ((clause 4 (3rd "="))
                   (if (clause 6 (5th "THEN"))
                       (add for-clauses (list 1st :of-type 2nd := 4th :then 6th))
                     (add for-clauses (list 1st :of-type 2nd := 4th))))
                  (t (go previous)))
            (go next-loop)
            previous
            (cond ((and (= (length clause) 3) (member 2nd '("PREV" "PREVIOUS") :test #'same))
                   (push (list 1st := nil :then 3rd) prev-clauses))
                  ((and (= (length clause) 4) (member 2nd '("PREV" "PREVIOUS") :test #'same))
                   (push (list 1st := 4th :then 3rd) prev-clauses))
                  ((and (= (length clause) 4) (member 3rd '("PREV" "PREVIOUS") :test #'same))
                   (push (list 1st :of-type 2nd := nil :then 4th) prev-clauses))
                  ((and (= (length clause) 5) (member 3rd '("PREV" "PREVIOUS") :test #'same))
                   (push (list 1st :of-type 2nd := 5th :then 4th) prev-clauses))
                  (t (go package)))
            (go next-loop)
            package
            (when (clause 4 (3rd "IN-PACKAGE"))
              (add for-clauses (list 2nd :being :each 1st :of 4th))
              (go next-loop))
            init-final-clauses
            (cond ((member 1st '("INIT" "INITIALLY") :test #'same)
                   (add init-clauses (cdr clause)))
                  ((member 1st '("FINAL" "FINALLY") :test #'same)
                   (setq final-clause `(return (progn ,@(cdr clause)))))
                  (t (go accumulate-clauses)))
            (go next-loop)
            accumulate-clauses
            (when (member 1st '("COUNT" "COUNTING" "SUM" "SUMMING" "COLLECT" "COLLECTING"
                                "APPEND" "APPENDING" "NCONC" "NCONCING"
                                "MAXIMIZE" "MAXIMIZING" "MINIMIZE" "MINIMIZING")
                          :test #'same)
              (cond ((clause 2) (add body-clauses (list 1st 2nd)))
                    ((clause 3) (add body-clauses (list 1st 2nd :into 3rd)))
                    ((clause 4 (3rd "INTO")) (add body-clauses (list 1st 2nd :into 4th)))
                    (t (go do-clauses)))
              (go next-loop))
            (when (same 1st "MULTIPLY")
              (cond ((clause 2) (let ((accumulator (gensym "ACCUMULATOR-")))
                                  (add with-clauses (list accumulator := 1))
                                  (add body-clauses (list :do `(setq ,accumulator (* ,accumulator ,2nd))))
                                  (unless final-clause
                                    (setq final-clause (list 'return accumulator)))))
                    ((clause 3)
                     (add with-clauses (list 3rd := 1))
                     (add body-clauses (list :do `(setq ,3rd (* ,3rd ,2nd))))
                     (unless final-clause
                       (setq final-clause (list 'return 3rd))))
                    ((clause 4 (3rd "INTO"))
                     (add with-clauses (list 4th := 1))
                     (add body-clauses (list :do `(setq ,3rd (* ,4th ,2nd))))
                     (unless final-clause
                       (setq final-clause (list 'return 4th))))
                    (t (go do-clauses)))
              (go next-loop))
            ;; 24Oct24: Add finders, similar with iterate.
            finders
            (when (and (>= (length clause) 3)
                       (member 1st '("FIND" "FINDING") :test #'same))
              (if (and (>= (length clause) 4)
                       (member 3rd '("MAX" "MAXIMIZE" "MAXIMIZING" "MIN" "MINIMIZE" "MINIMIZING") :test #'same))
                  (let ((peak-num (if (and (>= (length clause) 5) (listp 5th))
                                      (second 5th)
                                    (gensym "PEAK-NUM-")))
                        (peak-value (if (>= (length clause) 5)
                                        (if (listp 5th) (first 5th) 5th)
                                      (gensym "PEAK-VALUE-")))
                        (value (gensym "VALUE-")))
                    (add with-clauses (list peak-num :and peak-value :and value))
                    (add body-clauses
                         (list :do (list 'setq value (if (and (listp 4th) (eq (car 4th) 'function))
                                                         (funcall 4th 2nd)
                                                       4th))
                               :when (list 'or (list 'null peak-num)
                                           (list (if (member 3rd '("MAX" "MAXIMIZE" "MAXIMIZING") :test #'same)
                                                     '> '<)
                                                 value peak-num))
                               :do (list 'setq peak-num value peak-value 2nd) :end))
                    (unless final-clause
                      (setq final-clause (list 'return (list 'values peak-value peak-num)))))
                (progn
                  (if (and (listp 3rd)
                           (eq (car 3rd) 'function))
                      (add body-clauses (list :until `(funcall ,3rd ,2nd)))
                    (add body-clauses (list :until 3rd)))
                  (setq final-clause `(return ,2nd))))
              (go next-loop))
            1-arg-clauses
            (when (member 1st '("NAMED" "REPEAT" "UNTIL" "WHILE" "ALWAYS" "NEVER" "THEREIS")
                          :test #'same)
              (add body-clauses clause)
              (go next-loop))
            cond-clauses
            (when (>= (length clause) 3)
              (cond ((same (car clause) "WHEN")
                     ;; Using Backquote here will cause variable leaking problem on LispWorks
                     (add body-clauses (nconc (list :when 2nd) (for-collect-clauses (cddr clause) t) (list :end))))
                    ((same (car clause) "UNLESS")
                     (add body-clauses (nconc (list :unless 2nd) (for-collect-clauses (cddr clause) t) (list :end))))
                    ((and (= (length clause) 4) (same 1st "IF"))
                     (add body-clauses (nconc (list :if (second clause)) (for-collect-clauses (list (third clause)) t)
                                              (list :else) (for-collect-clauses (list (fourth clause)) t) (list :end))))
                    (t (go do-clauses)))
              (go next-loop))
            do-clauses
            (setq this-do-p t)
            (if last-do-p
                (add body-clauses (list clause))
              (add body-clauses (list :do clause)))
            next-loop
            (setq last-do-p this-do-p
                  this-do-p nil)
            (setq clauses (rest clauses)
                  clause (car clauses))
            (when clauses (go start-loop)))))
      (setq with-clauses (mapcan (lambda (list) (cons :with list)) with-clauses)
            for-clauses (mapcan (lambda (list) (cons :for list)) for-clauses))
      (when prev-clauses
        (setq for-clauses (nconc (loop for i from 0
                                       for forms in prev-clauses
                                       if (= i 0)
                                         collect :for
                                       else collect :and
                                       nconc forms)
                                 for-clauses)))
      (nconc (when init-clauses (list :init (list 'progn init-clauses)))
             with-clauses for-clauses (apply #'nconc body-clauses)
             (when final-clause (list :finally final-clause))))))

(defmacro for (&body body)
  "FOR macro that expands to LOOP.

Example:

```common-lisp
(for (i range 1 10)
     (prev previous i 0)
     (collect (list prev i)))
```"
  (cons 'cl:loop (for-collect-clauses body)))

(defun for*-collect-clauses (body)
  (let ((first-clause (for-collect-clauses (list (first body)))))
    (if (member (first first-clause) '(:do :if :when :unless
                                        :while :until :always :never :thereis :named
                                        :collect :nconc :append :sum :count :maximize :minimize))
        (values (for-collect-clauses body) nil)
      (multiple-value-bind (rest-clauses next-level-p)
          (for*-collect-clauses (rest body))
        (if next-level-p
            (values `(cl:loop ,@first-clause :do ,rest-clauses) t)
          (values `(cl:loop ,@first-clause ,@rest-clauses) t))))))

(defmacro for* (&body body)
  "Easy nested FOR loop.

Each variable stepping clause will establish a new loop level.

Example:

```common-lisp
(let ((arr (make-array '(10 10))))
  (for* (i :range 10)
        (j :range 10)
        (setf (aref arr i j) 0)))
```"
  (for*-collect-clauses body))

(export '(for for* *for-extend-clauses-alist* define-for-extend-clause))

#+lispworks
(editor:setup-indent "for" 0 5)
#+lispworks
(editor:setup-indent "for*" 0 6)
