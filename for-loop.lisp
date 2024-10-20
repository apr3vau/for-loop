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
true if the form is matched, then the yield function should return a
list of symbols to be inserted into final `LOOP'."
  (push (cons predicate-function yield-function) *for-extend-clauses-alist*))

(defun for-collect-clauses (clauses &optional (andp))
  (let* ((r (list nil))
         (tail r)
         last-do-p this-do-p)
    (flet ((collect (&rest items)
             (dolist (i (if (and andp (cdr r) (null this-do-p))
                            (cons :and items)
                          items))
               (rplacd tail (setf tail (list i)))))
           (same (s1 s2)
             (when (and (or (symbolp s1) (stringp s1))
                        (or (symbolp s2) (stringp s2)))
               (string-equal s1 s2))))
      (declare (inline collect))
      (symbol-macrolet ((1st (first clause))
                        (2nd (second clause))
                        (3rd (third clause))
                        (4th (fourth clause))
                        (5th (fifth clause))
                        (6th (sixth clause))
                        (7th (seventh clause)))
        (macrolet ((clause (clause-length &rest sames)
                     (nconc `(and (= (length clause) ,clause-length))
                            (loop for (val1 val2) in sames collect `(same ,val1 ',val2)))))
          (prog ((clause (car clauses)))
            start-loop
            ;; extensions
            (loop for (pred . yield) in *for-extend-clauses-alist*
                when (funcall pred clause)
                    do (apply #'collect (funcall yield clause))
                       (go next-loop))
            ;; range
            (cond ((and (>= (length clause) 2) (same 2nd :range))
                   (cond ((clause 2)
                          (collect :for 1st :from 0))
                         ((clause 4 (3rd by))
                          (collect :for 1st :from 0 :by 4th))
                         ((clause 3)
                          (collect :for 1st :to `(1- ,3rd)))
                         ((clause 4)
                          (collect :for 1st :from 3rd :to 4th))
                         ((clause 5 (4th by))
                          (collect :for 1st :to `(1- ,3rd) :by 5th))
                         ((clause 5)
                          (collect :for 1st :from 3rd :to 4th :by 5th))
                         ((clause 6 (5th by))
                          (collect :for 1st :from 3rd :to 4th :by 6th))
                         (t (go from-to))))
                  ((and (>= (length clause) 3) (same 3rd :range))
                   (cond ((clause 3)
                          (collect :for 1st :of-type 2nd :from 0))
                         ((clause 5 (4th by))
                          (collect :for 1st :of-type 2nd :from 0 :by 5th))
                         ((clause 4)
                          (collect :for 1st :of-type 2nd :to 3rd))
                         ((clause 5)
                          (collect :for 1st :of-type 2nd :from 4th :to 5th))
                         ((clause 6 (5th by))
                          (collect :for 1st :of-type 2nd :from 4th :by 6th))
                         ((clause 6)
                          (collect :for 1st :of-type 2nd :from 4th :to 5th :by 6th))
                         ((clause 7 (6th by))
                          (collect :for 1st :of-type 2nd :from 4th :to 5th :by 7th))
                         (t (go from-to))))
                  (t (go from-to)))
            (go next-loop)
            from-to
            (when (and (> (length clause) 3) (or (member 2nd '("FROM" "TO" "BY") :test #'same)))
              (apply #'collect (cons :for clause))
              (go next-loop))
            (when (and (> (length clause) 4) (or (member 3rd '("FROM" "TO" "BY") :test #'same)))
              (apply #'collect (nconc (list :for 1st :of-type 2nd) (cddr clause)))
              (go next-loop))
            lists
            (cond ((clause 4 (3rd in-plist))
                   (collect :for (list 1st 2nd) :on 4th :by #'cddr))
                  ((clause 4 (3rd in-alist))
                   (collect :for (cons 1st 2nd) :on 4th))
                  (t (go tables)))
            (go next-loop)
            tables
            (when (>= (length clause) 3)
              (cond ((member 2nd '("HASH-KEY" "HASH-KEYS" "IN-HASH-KEY" "IN-HASH-KEYS" "TABLE-KEYS") :test #'same)
                     (collect :for 1st :being :each :hash-key :of 3rd))
                    ((member 2nd '("HASH-VALUE" "HASH-VALUES" "IN-HASH-VALUE" "IN-HASH-VALUES" "TABLE-VALUES") :test #'same)
                     (collect :for 1st :being :each :hash-value :of 3rd))
                    ((and (clause 4) (member 3rd '("TABLE" "HASHTABLE" "TABLE-PAIRS" "HASH-TABLE" "IN-TABLE" "IN-HASHTABLE" "IN-HASH-TABLE") :test #'same))
                     (collect :for 1st :being :each :hash-key :of 4th :for 2nd := `(gethash ,1st ,4th)))
                    (t (go in)))
              (go next-loop))
            in
            (cond ((same 2nd 'in)
                   (cond ((clause 5 (4th by))
                          (collect :for 1st :in 3rd :by 5th))
                         ((clause 4)
                          (collect :for 1st :in 3rd :by 4th))
                         (t (collect :for 1st :in 3rd))))
                  ((same 3rd 'in)
                   (cond ((clause 6 (5th by))
                          (collect :for 1st :of-type 2nd :in 4th :by 6th))
                         ((clause 5)
                          (collect :for 1st :of-type 2nd :in 4th :by 5th))
                         (t (collect :for 1st :of-type 2nd :in 4th))))
                  ((same 2nd 'on)
                   (cond ((clause 5 (4th by))
                          (collect :for 1st :on 3rd :by 5th))
                         ((clause 4)
                          (collect :for 1st :on 3rd :by 4th))
                         (t (collect :for 1st :on 3rd))))
                  ((same 3rd 'on)
                   (cond ((clause 6 (5th by))
                          (collect :for 1st :of-type 2nd :on 4th :by 6th))
                         ((clause 5)
                          (collect :for 1st :of-type 2nd :on 4th :by 5th))
                         (t (collect :for 1st :of-type 2nd :on 4th))))
                  (t (go across)))
            (go next-loop)
            across
            (cond ((clause 3 (2nd across))
                   (collect :for 1st :across 3rd))
                  ((clause 4 (3rd across))
                   (collect :for 1st :of-type 2nd :across 4th))
                  (t (go equal)))
            (go next-loop)
            equal
            (cond ((clause 4 (3rd then))
                   (collect :for 1st := 2nd :then 4th))
                  ((clause 5 (4th then))
                   (collect :for 1st :of-type 2nd := 3rd :then 5th))
                  ((clause 3 (2nd =))
                   (if (clause 5 (4th then))
                       (collect :for 1st := 3rd :then 5th)
                     (collect :for 1st := 3rd)))
                  ((clause 4 (3rd =))
                   (if (clause 6 (5th then))
                       (collect :for 1st :of-type 2nd := 4th :then 6th)
                     (collect :for 1st :of-type 2nd := 4th)))
                  (t (go previous)))
            (go next-loop)
            previous
            (cond ((clause 3 (2nd previous))
                   (setf (cdr r) (nconc (list :for 1st := nil :then 3rd) (cdr r))))
                  ((clause 4 (2nd previous))
                   (setf (cdr r) (nconc (list :for 1st := 4th :then 3rd) (cdr r))))
                  ((clause 4 (3rd previous))
                   (setf (cdr r) (nconc (list :for 1st :of-type 2nd := nil :then 4th) (cdr r))))
                  ((clause 5 (3rd previous))
                   (setf (cdr r) (nconc (list :for 1st :of-type 2nd := 5th :then 4th) (cdr r))))
                  (t (go package)))
            (go next-loop)
            package
            (when (clause 4 (3rd in-package))
              (collect :for 2nd :being :each 1st :of 4th)
              (go next-loop))
            init-final-clauses
            (cond ((member 1st '("INIT" "INITIALLY") :test #'same)
                   (collect :initially (cons 'progn (cdr clause))))
                  ((member 1st '("FINAL" "FINALLY") :test #'same)
                   (collect :finally `(return (progn ,@(cdr clause)))))
                  (t (go accumulate-clauses)))
            (go next-loop)
            accumulate-clauses
            (when (member 1st '("COUNT" "COUNTING" "SUM" "SUMMING" "COLLECT" "COLLECTING"
                                "APPEND" "APPENDING" "NCONC" "NCONCING"
                                "MAXIMIZE" "MAXIMIZING" "MINIMIZE" "MINIMIZING")
                          :test #'same)
              (cond ((clause 2) (collect 1st 2nd))
                    ((clause 3) (collect 1st 2nd :into 3rd))
                    ((clause 4 (3rd into)) (collect 1st 2nd :into 4th))
                    (t (go do-clauses)))
              (go next-loop))
            1-arg-clauses
            (when (member 1st '("NAMED" "REPEAT" "UNTIL" "WHILE" "ALWAYS" "NEVER" "THEREIS")
                          :test #'same)
              (apply #'collect clause)
              (go next-loop))
            cond-clauses
            (when (>= (length clause) 3)
              (cond ((same (car clause) "WHEN")
                     (apply #'collect `(:when ,2nd ,@(for-collect-clauses (cddr clause) t) :end)))
                    ((same (car clause) "UNLESS")
                     (apply #'collect `(:unless ,2nd ,@(for-collect-clauses (cddr clause) t) :end)))
                    ((and (= (length clause) 4) (same 1st "IF"))
                     (apply #'collect `(:if ,(second clause) ,@(for-collect-clauses (list (third clause)) t)
                                         :else ,@(for-collect-clauses (list (fourth clause)) t) :end)))
                    (t (go do-clauses)))
              (go next-loop))
            do-clauses
            (setq this-do-p t)
            (if last-do-p
                (collect clause)
              (collect :do clause))
            next-loop
            (setq last-do-p this-do-p
                  this-do-p nil)
            (setq clauses (rest clauses)
                  clause (car clauses))
            (when clauses (go start-loop)))))
      (cdr r))))

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
