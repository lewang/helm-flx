;;; helm-flx.el --- Flx matching with helm.

;; Copyright © 2014 Le Wang

;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: Use flx algorithm for fuzzy helm matching.
;; Created: 2014-12-27
;; Version: 0.1
;; URL: https://github.com/lewang/helm-flx.el
;; Package-Requires: ((heap "0.3"))

;; This file is NOT part of GNU Emacs.

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;
;;
;;

;;; Installation:

;;
;;
;;

;; Add the following code to your init file:
;;
;;

;;; Code:

(eval-when-compile (require 'cl))

(require 'heap)
(require 'helm)

(require 'flx)

(defface helm-flx-score
  '((t (:inherit helm-match :height 0.7 :slant italic)))
  "Face used to show flx score."
  :group 'helm-match-plugin)

(defvar helm-flx--saved nil
  "Saved values of `helm-fuzzy-sort-fn' and `helm-fuzzy-matching-highlight-fn'")

(defsubst helm-flx-display (candidate)
  (car candidate))
(defsubst helm-flx-real (candidate)
  (cadr candidate))
(defsubst helm-flx-scores (candidate)
  (caddr candidate))
(defsubst helm-flx-score (candidate)
  (car (helm-flx-scores candidate)))


(defun helm-flx-sorted-top-k (list k comparator)
  "Return sorted K elements from list according to comparator."
  (if (= k 0)
      nil
    (let ((heap (make-heap comparator k)))
      (cl-loop for i below (length list)
            for item in list
            do (cond ((< i k)
                      (heap-add heap item))
                     ((funcall comparator (heap-root heap) item)
                      (heap-delete-root heap)
                      (heap-add heap item))))
      (nreverse
       (cl-loop for i below (heap-size heap)
             collect (heap-delete-root heap))))))

(defsubst helm-flx-pattern ()
  "Get first part of `helm-pattern' used for fuzzy matching."
  (substring helm-pattern 0 (string-match "\\s-" helm-pattern)))

(defun helm-flx-comparator (c1 c2)
  "Internal use for sorting."
  (< (helm-flx-score c1) (helm-flx-score c2)))

(defun helm-flx-sort (candidates _source &optional use-real)
  "The flx sort function for helm.
Match info is attached to the real candidate, which get removed
in the highlighter."
  (let ((pattern (helm-flx-pattern)))
    (if (string= pattern "")
        candidates
      (let* ((cache (if (equal (car (helm-actions-from-type-file))
                               (cdr-safe (assq 'actions (helm-get-current-source))))
                        flx-file-cache
                      flx-strings-cache))
             (scored-candidates
              (mapcar
               (lambda (c)
                 (let* ((cand (if (consp c)
                                  (if use-real (cdr c) (car c))
                                c))
                        (scr (flx-score cand pattern cache)))
                   `(,(if (consp c) (car c) c)
                     ,(if (consp c) (cdr c) c)
                     ,scr)))
               candidates))
             (top-candidates (helm-flx-sorted-top-k scored-candidates helm-candidate-number-limit #'helm-flx-comparator)))
        (cl-loop for candidate in top-candidates
                 collect (helm-flx-highlight-match candidate use-real))))))

(defun helm-flx-highlight-match (candidate use-real)
  "Highlight display portion of match with flx data."
  (let* ((real (helm-flx-real candidate))
         (display (helm-flx-display candidate)))
    (cons
     (if use-real
         (let ((base (cl-search real display)))
           (if base
               (helm-flx-propertize display (helm-flx-scores candidate) base)
             (helm-flx-propertize display (helm-flx-scores candidate) -1)))
       (helm-flx-propertize display (helm-flx-scores candidate) 0))
     real)))

(defun helm-flx-propertize (str score-data base)
  "Return propertized copy of obj according to score.

A BASE value of -1 means to not propertize str at all, but the
match score still may be prefixed."
  (if (null score-data)
      str
    (let* ((pad (format "[%s] " (propertize (number-to-string (car score-data)) 'face 'helm-flx-score)))
           (result (concat pad str)))
      (if (= base -1)
          result
        (let* ((offset (+ base (length pad)))
               (matches (mapcar (lambda (num)
                                 (+ num offset))
                               (cdr score-data)))
               (block-started (car matches))
               (last-char nil)
               (face 'helm-match))
          (cl-loop for char in matches
                   do (progn
                        (when (and last-char
                                 (not (= (1+ last-char) char)))
                          (put-text-property block-started  (1+ last-char) 'face face result)
                          (setq block-started char))
                        (setq last-char char)))
          (put-text-property block-started  (1+ last-char) 'face face result))
        result))))

(define-minor-mode helm-flx-mode
  "Toggle flx algorithm for helm fuzzy matching."
  :group 'helm
  (if helm-flx-mode
      (progn
        (setq helm-flx--saved (cons (symbol-function 'helm-fuzzy-matching-default-sort-fn) (symbol-function 'helm-fuzzy-default-highlight-match)))
        (fset 'helm-fuzzy-matching-default-sort-fn (symbol-function 'helm-flx-sort))
        (fset 'helm-fuzzy-default-highlight-match (lambda (_candidate) _candidate)))
    (fset 'helm-fuzzy-matching-default-sort-fn (car helm-flx--saved))
    (fset 'helm-fuzzy-default-highlight-match (cdr helm-flx--saved))))



(provide 'helm-flx)

;;; helm-flx.el ends here
