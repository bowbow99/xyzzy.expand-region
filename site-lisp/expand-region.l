;;; -*- mode: lisp; package: expand-region -*-
;;;
;;; expand-region.l -- てきとーにリージョンを広げる

;;; Code:

;;;; Package

(in-package :editor)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (export '(expand-region
            contract-region
            )))

(defpackage :expand-region
  (:nicknames "er")
  (:use :lisp :editor)
  )

(in-package :expand-region)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (export '(region/word region/symbol region/sexps
            region/whole-string region/inside-of-string
            )))



;;;; Utilities

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun sym (name-format &rest args)
    (intern (apply #'format nil name-format args))))


(defun print-structure-object-readably (obj out)
  (let ((def (si:*structure-definition obj)))
    (format out "#S(~S" (si:*structure-definition-name def))
    (dotimes (i (si:*structure-definition-nslots def))
      (format out " ~S ~S"
              (si:*structure-definition-slot-description def i)
              (si:*index-slot-value obj i)))
    (format out ")")))

(defmacro with-$gensyms (&body body)
  (let ((known-gensyms (make-hash-table)))
    (labels ((gensymp (x)
               (and (symbolp x)
                    (char= (char (symbol-name x) 0) #\$)))
             (get-gensym (sym)
               (or #1=(gethash sym known-gensyms)
                   (setf #1# (gensym (format nil "~A-" (subseq (symbol-name sym) 1))))))
             (replace-gensyms (form)
               (cond ((gensymp form)
                       (get-gensym form))
                     ((consp form)
                       (cons (replace-gensyms (car form))
                             (replace-gensyms (cdr form))))
                     (t
                       form))))
      `(progn ,@(replace-gensyms body)))))

(defmacro awhen (condition &body body)
  `(let ((it ,condition))
     (if it (progn ,@body))))


;;;; Semantic Region Object

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun print-semantic-region-object (r out n)
    (if *print-readably*
      (print-structure-object-readably r out)
      (let ((start (semantic-region-start r))
            (end   (semantic-region-end r))
            (tag   (semantic-region-tag r)))
        (format out "#<~S#~S (~S-~S) ~S>"
                'semantic-region
                tag start end
                (buffer-substring start end))))))

(defstruct (semantic-region
            (:print-function print-semantic-region-object))
  start end tag)

(defun semantic-region-smaller (a b)
  (or (> (semantic-region-start a)
         (semantic-region-start b))
      (< (semantic-region-end a)
         (semantic-region-end b))))

(defun semantic-region-equal (a b)
  (and (= (semantic-region-start a)
          (semantic-region-start b))
       (= (semantic-region-end a)
          (semantic-region-end b))))

(defun regionize-semantic-region-forward (x)
  (goto-char (semantic-region-start x))
  (set-mark-command)
  (goto-char (semantic-region-end x)))





;;;; Semantic Region Finder

(defvar-local -find-semantic-region-functions-
    nil
  "*リージョンを見つける関数のリスト。
関数は引数なしで呼び出され、以下の3つの値を返さなければならない。
START -- リージョンの開始位置
END   -- リージョンの終了位置
TAG   -- リージョンを識別する何か")

(defmacro find-semantic-region (tag &body body)
  (with-$gensyms
   `(handler-case
        (save-excursion
          (let (($start nil)
                ($end   nil))
            (labels ((mark-start (&optional (point (point)))
                       (setf $start point))
                     (mark-end (&optional (point (point)))
                       (setf $end point))
                     ($return (&key (start $start) (end $end))
                       (make-semantic-region
                        :start start :end end :tag ',tag)))
              ,@body
              (cond ((and $start        $end)
                      ($return))
                    ((and $start        (null $end))
                      ($return :end (point)))
                    ((and (null $start) $end)
                      ($return :start (point)))
                    ((and (null $start) (null $end))
                      nil)))))
      (quit () nil))))


(defmacro define-semantic-region (tag &body body)
  `(defun ,(sym "region/~A" tag) ()
     (find-semantic-region ,tag
       ,@body)))

(defmacro define-syntactic-region (tag syntax-spec)
  `(define-semantic-region ,tag
     (when (null (parse-point-syntax))
       (skip-syntax-spec-backward ,syntax-spec)
       (mark-start)
       (skip-syntax-spec-forward ,syntax-spec))))

(define-syntactic-region word "w")
(define-syntactic-region symbol "w_")

(defun beginning-of-string ()
  (while (eql (parse-point-syntax) :string)
    (backward-char)))

(defun end-of-string ()
  (while (eql (parse-point-syntax) :string)
    (forward-char)))

(define-semantic-region inside-of-string
  (when (eql (parse-point-syntax) :string)
    (beginning-of-string)
    (forward-char)
    (mark-start)
    (end-of-string)
    (backward-char)))

(define-semantic-region whole-string
  (when (eql (parse-point-syntax) :string)
    (beginning-of-string)
    (mark-start)
    (forward-char)
    (end-of-string)))

(defun region/sexps ()
  (save-excursion
    (labels ((new-region (start end)
               (make-semantic-region :tag 'sexp :start start :end end)))
      (let ((sexp-regions nil))
        (while (up-list -1 t)
          (let ((start (point))
                (end   (progn
                         (forward-sexp 1 t)
                         (point))))
            (push (new-region start end) sexp-regions)
            (push (new-region (1+ start) (1- end)) sexp-regions)))
        sexp-regions))))

(setq-default -find-semantic-region-functions-
              '(region/word
                region/symbol
                region/sexps
                region/inside-of-string
                region/whole-string
                ))

(defvar-local -semantic-regions- nil)



(defun %find-semantic-region (finder)
  (ignore-errors
   (funcall finder)))

(defun %list-semantic-regions ()
  (let ((regions nil))
    (dolist (finder -find-semantic-region-functions-)
      (awhen (%find-semantic-region finder)
        (cond ((semantic-region-p it)
                (pushnew it regions :test #'semantic-region-equal))
              ((listp it)
                (dolist (region it)
                  (pushnew region regions :test #'semantic-region-equal))))))
    regions))


(defun list-semantic-regions ()
  (sort (%list-semantic-regions) #'semantic-region-smaller))

(defvar *semantic-regions* nil)

(defun %regionize-nth-semantic-region ()
  (regionize-semantic-region-forward
   (nth (car *semantic-regions*)
        (cdr *semantic-regions*))))




(defun expand-region (&optional arg)
  (interactive)
  (if *semantic-regions*
    (unless (>= (car *semantic-regions*)
                (1- (length (cdr *semantic-regions*))))
      (incf (car *semantic-regions*)))
    (setf *semantic-regions* (cons 0 (list-semantic-regions))))
  (%regionize-nth-semantic-region))

(defun contract-region (&optional arg)
  (interactive)
  (decf (car *semantic-regions*))
  (%regionize-nth-semantic-region))

(global-set-key #\C-\, 'expand-region)
(global-set-key #\C-\. 'contract-region)

(defun try-find-semantic-region (finder)
  (interactive "Sfinder> ")
  (let ((*package* (find-package :expand-region)))
    (msgbox "finder: ~S~%=> ~A"
            finder
            (%find-semantic-region finder))))

(global-set-key '(#\C-c #\t) 'try-find-semantic-region)

(defun clear-semantic-regions ()
  (unless (member *this-command* '(expand-region contract-region))
    (setf *semantic-regions* nil)))
(add-hook '*pre-command-hook* 'clear-semantic-regions)




(pushnew 'expand-region user::*rv-start-command-list*)
(pushnew 'contract-region user::*rv-start-command-list*)




(provide "expand-region")
;;; expand-region.l ends here.
