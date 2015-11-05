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



;;;; Semantic Region Finder

(defvar-local -find-semantic-region-functions-
    nil
  "*リージョンを見つける関数のリスト。
関数は引数なしで呼び出され、以下の3つの値を返さなければならない。
START -- リージョンの開始位置
END   -- リージョンの終了位置
TAG   -- リージョンを識別する何か")

(defmacro find-semantic-region (tag &body body)
  `(handler-case
       (let ((#1=#:start nil)
             (#2=#:end   nil))
         (labels ((mark-start (&optional (point (point)))
                    (setf #1# point))
                  (mark-end (&optional (point (point)))
                    (setf #2# point)))
           ,@body
           (cond ((and (null #1#) #2#)
                   (mark-start))
                 ((and #1# (null #2#))
                   (mark-end)))
           (unless (or (null #1#)
                       (eql #1# #2#))
             (values #1# #2# ',tag))))
     (quit ()
       nil)))

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
    (backward-char))) "abc"

(define-semantic-region whole-string
  (when (eql (parse-point-syntax) :string)
    (beginning-of-string)
    (mark-start)
    (forward-char)
    (end-of-string)))


(define-semantic-region inside-of-sexp
  (unless (and (up-list -1 t)
               (mark-start (1+ (point)))
               (goto-matched-parenthesis))
    (quit)))

(define-semantic-region whole-sexp
  (unless (and (up-list -1 t)
               (mark-start)
               (goto-matched-parenthesis)
               (forward-char))
    (quit)))

(setq-default -find-semantic-region-functions-
              '(region/word
                region/symbol
                region/inside-of-sexp
                region/whole-sexp
                region/inside-of-string
                region/whole-string
                ))

(defvar-local -semantic-regions- nil)



(defun %find-semantic-region (finder)
  (ignore-errors
   (multiple-value-bind (start end tag)
       (save-excursion (funcall finder))
     (when (and start
                end)
       (make-semantic-region
        :start start :end end :tag tag)))))

(defun %list-semantic-regions ()
  (let ((regions nil))
    (dolist (finder -find-semantic-region-functions-)
      (let ((it (%find-semantic-region finder)))
        (when it
          (push it regions))))
    regions))

(defun list-semantic-regions ()
  (sort (%list-semantic-regions) #'semantic-region-smaller))

(defun expand-region (&optional arg)
  (interactive)
  (let ((*package* (find-package :expand-region)))
    (msgbox "~{~A~%~}" (list-semantic-regions))))

(global-set-key #\C-\, 'expand-region)


(defun try-find-semantic-region (finder)
  (interactive "Sfinder> ")
  (msgbox "finder: ~S~%=> ~A"
          finder
          (%find-semantic-region finder)))

(global-set-key '(#\C-c #\t) 'try-find-semantic-region)



;aaaaa-bbbb








(provide "expand-region")
;;; expand-region.l ends here.
