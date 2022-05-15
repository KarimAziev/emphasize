;;; emphasize.el --- Configure emphasize -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/emphasize
;; Keywords: convenience
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:

;; This file configures operations with emphasize

;; Commands

;; M-x `emphasize-thing-at-point'
;;      Emphasize thing at point or active region.
;;      You can customize which chars to insert with `emphasize-modes-alist',
;;      and which chars to use to guess thing at point - with
;;      `emphasize-thing-at-point-chars'.

;; Customization

;; `emphasize-thing-at-point-chars'
;;      Alist of major modes and regexp of thing to wrap.

;; `emphasize-modes-alist'
;;      Aist of major modes and corresponding chars to insert before and after.

;;; Code:



(defcustom emphasize-modes-alist '((org-mode
                                    ("~" . "~")
                                    ("*" . "*")
                                    ("/" . "/")
                                    ("_" . "_")
                                    ("=" . "=")
                                    ("+" . "+")
                                    ("`" . "'"))
                                   (emacs-lisp-mode
                                    ("(" . ")")
                                    ("'" . "")
                                    ("\"" . "\"")))
  "Aist of major modes and corresponding chars to insert before and after."
  :type '(alist :key-type (symbol :tag "Major mode")
                :value-type (alist
                             :key-type (string :tag "Open char")
                             :value-type (string :tag "Closed char")))
  :group 'emphasize)

(defcustom emphasize-thing-at-point-chars '((org-mode . "-\"'/$A-Za-zА-Яа-я0-9:.")
                                            (emacs-lisp-mode . "-\"'/$A-Za-z0-9:."))
  "Alist of major modes and regexp of thing to wrap."
  :type '(alist :key-type (symbol :tag "Major mode")
                :value-type (regexp :tag "Regexp"))
  :group 'emphasize)

(defun emphasize-read (chars-alist word-re)
  "Emphasize thing at point that matches WORD-RE with CHARS-ALIST.
CHARS-ALIST is alist of opened and closed chars to insert."
  (let ((open-chars (mapcar 'car chars-alist))
        (closed-chars (mapcar 'cdr chars-alist))
        (bounds)
        (word)
        (variants))
    (setq word-re
          (replace-regexp-in-string
           (mapconcat 'regexp-quote
                      (seq-uniq (seq-remove
                                 'string-empty-p
                                 (append open-chars closed-chars)))
                      "\\|")
           ""
           word-re))
    (save-excursion
      (cond ((and (region-active-p)
                  (use-region-p))
             (setq bounds
                   (cons (region-beginning)
                         (region-end)))
             (setq word (buffer-substring-no-properties
                         (car bounds)
                         (cdr bounds)))
             (setq variants (mapcar (lambda (it) (concat (car it)
                                                    word
                                                    (cdr it)))
                                    chars-alist)))
            ((looking-at (concat "[" word-re "]"))
             (setq bounds
                   (cons
                    (save-excursion
                      (skip-chars-backward word-re)
                      (point))
                    (save-excursion
                      (skip-chars-forward word-re)
                      (point))))
             (setq word (buffer-substring-no-properties
                         (car bounds)
                         (cdr bounds)))
             (setq variants (mapcar (lambda (it) (concat (car it) word (cdr it)))
                                    chars-alist)))
            ((looking-at (mapconcat 'regexp-quote open-chars "\\|"))
             (let ((open-char (match-string-no-properties 0))
                   (beg (point))
                   (end)
                   (w-beg)
                   (w-end)
                   (closed-char))
               (forward-char (length open-char))
               (setq w-beg (point))
               (skip-chars-forward word-re)
               (setq w-end (point))
               (setq word (buffer-substring-no-properties w-beg w-end))
               (when (looking-at (mapconcat 'regexp-quote closed-chars "\\|"))
                 (setq closed-char (match-string-no-properties 0))
                 (setq end (+ (point) (length closed-char)))
                 (setq bounds (cons beg end))
                 (let ((el (seq-find (lambda (it) (and (equal (car it) open-char)
                                                  (equal (cdr it) closed-char)))
                                     chars-alist)))
                   (setq variants (mapcar (lambda (it) (concat
                                                   (car it) word (cdr it)))
                                          (remove el chars-alist)))
                   (setq variants (push word variants))))))))
    (when (and variants bounds)
      (let ((choice (completing-read (format "Replace %s with:"
                                             (buffer-substring-no-properties
                                              (car bounds)
                                              (cdr bounds)))
                                     variants)))
        (replace-region-contents (car bounds) (cdr bounds)
                                 (lambda () choice))))))

(defun emphasize-thing-at-point ()
  "Emphasize thing at point or active region.
You can customize which chars to insert with `emphasize-modes-alist',
and which chars to use to guess thing at point - with
`emphasize-thing-at-point-chars'."
  (interactive)
  (emphasize-read (or (alist-get major-mode emphasize-modes-alist)
                      '("\"" . "\""))
                  (or (alist-get major-mode emphasize-thing-at-point-chars)
                      "-\"'$A-Za-zА-Яа-я0-9:.")))

(provide 'emphasize)
;;; emphasize.el ends here