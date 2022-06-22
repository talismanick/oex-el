;;; oex.el --- open files externally, by extension -*- lexical-binding: t -*-

;; Author: Nick Talin
;; Maintainer: Nick Talin
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/talismanick/oex-el
;; Keywords: open, external, file, process


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Open files whose extensions match in `oex-assoc' with the paired
;; program, using `start-process' to avoid `async-shell-command'
;; buffers.  Derived from `openwith'.  Written for Unix users.

;;; Code:
(defgroup oex nil
  "Open files externally."
  :group 'files
  :group 'processes
  :prefix "oex-")
(defcustom oex-assoc '((("zathura" "--fork") "pdf" "eps" "ps")
                       (("mpv" "--ontop") "mp4" "mpeg" "mkv" "avi" "wmv"))
  "Programs (and their options) paired with file extensions."
  :group 'oex
  :type '(repeat (list (list (string :tag "Program & Options"))
                       (list (string :tag "Extensions")))))
(defcustom oex-confirm nil
  "Ask for confirmation before invoking external programs."
  :group 'oex
  :type 'boolean)
(defun oex--run (op &rest args)
  "If file at head of ARGS matches `oex-assoc', run externally; else, apply OP."
  (when (and (bound-and-true-p oex-mode) (not (buffer-modified-p)) (zerop (buffer-size)))
    (dolist (i (let (l) (dolist (s oex-assoc l) ;; i = (("program" "opt1"...) . "regex")
                          (push (cons (car s) (concat "\\." (regexp-opt (cdr s))  "$")) l))))
      (when (and (save-match-data (string-match (cdr i) (car args)))
                 (or (not oex-confirm) (y-or-n-p (format "%s %s? " (caar i) (car args)))))
        (let ((arg-expand (list (expand-file-name (car args)))))
          (dolist (opt (cdar i)) (push opt arg-expand))
          (apply #'start-process (caar i) nil (caar i) arg-expand))
        (kill-buffer nil)
        (when (featurep 'recentf) (recentf-add-file (car args)))
        ;; Avoid fallthrough to other handlers if matched and run.
        (error "Opened %s in external program" (file-name-nondirectory (car args))))))
  (let ((inhibit-file-name-handlers
         (cons 'oex--run (and (eq inhibit-file-name-operation op) inhibit-file-name-handlers)))
        (inhibit-file-name-operation op))
    (apply op args)))
;;;###autoload
(define-minor-mode oex-mode
  "Automatically open files with external programs."
  :lighter ""
  :global t
  (if oex-mode
      (progn
        ;; register `oex--run' for all files
        (put 'oex--run 'safe-magic t)
        (put 'oex--run 'operations '(insert-file-contents))
        (push '("" . oex--run) file-name-handler-alist))
    (rassq-delete-all 'oex--run file-name-handler-alist)))
(provide 'oex)

;;; oex.el ends here
