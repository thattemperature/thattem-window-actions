;;; Window-actions --- controls actions of window showing  -*- lexical-binding: t; -*-

;; Author: That Temperature <2719023332@qq.com>
;; Package-Requires: ((cond-let "1.1.1") thattem-mode-line)
;; URL: https://github.com/thattemperature/thattem-window-actions

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

;; This package defines major mode based window actions.
;; For example, show buffer in side window with specified width/height.
;; This is in cooperation with mode-specific mode/header line format.

;;; Code:

(defgroup thattem-window-actions nil
  "Modified window display actions."
  :group 'convenience
  :group 'thattem)

(require 'cond-let)
(require 'thattem-mode-line)

;;; Define some classes of major mode.

(defcustom thattem-shell-like-modes
  '(eshell-mode
    shell-mode
    term-mode
    inferior-python-mode
    compilation-mode
    process-menu-mode)
  "A list of major mode which should be seen as shell."
  :type '(repeat symbol)
  :group 'thattem-window-actions)

(defcustom thattem-help-modes
  '(help-mode
    Info-mode
    messages-buffer-mode
    fanyi-mode
    sdcv-mode
    shortdoc-mode)
  "A list of major mode which should be seen as helper."
  :type '(repeat symbol)
  :group 'thattem-window-actions)

(defcustom thattem-fundamental-modes
  '(fundamental-mode)
  "A list of major mode which is fundamental mode."
  :type '(repeat symbol)
  :group 'thattem-window-actions)

(defcustom thattem-special-mode-list
  (list thattem-shell-like-modes
        thattem-help-modes
        thattem-fundamental-modes)
  "A list of all above mode lists."
  :type '(repeat (repeat symbol))
  :group 'thattem-window-actions)

;;; Define helper functions.

(defun thattem--mode-list-for-buffer-match (mode-list)
  "A helper function to build cons-cell for \\='buffer-match-p\\='.
Which matches the buffer with the major mode derived from one of the
MODE-LIST.  You should \\='cons\\=' a symbol \"or\" to the left of
the return value when used."
  (if mode-list
      (cons (cons 'derived-mode (car mode-list))
            (thattem--mode-list-for-buffer-match (cdr mode-list)))
    nil))

;;; Define new value of variable "display-buffer-alist"

(defvar thattem-display-buffer-alist-default
  (let ((shell-like-action
         `((display-buffer-reuse-mode-window
            display-buffer-in-side-window)
           (mode . ,thattem-shell-like-modes)
           (side . bottom)
           (slot . 0)
           (window-height . 10)
           (preserve-size . (nil . t))
           (window-parameters
            . ((no-other-window . t)))))
        (help-action
         `((display-buffer-reuse-mode-window
            display-buffer-in-side-window)
           (mode . ,thattem-help-modes)
           (side . right)
           (slot . 0)
           (window-width . 40)
           (preserve-size . (t . nil))
           (window-parameters
            . ((no-other-window . t))))))
    ;; shell-like regexp settings
    `((,(concat "\\(^\\*eshell\\*\\(<[[:digit:]]+>\\)?$\\)\\|"
                "\\(^\\*shell\\*\\(<[[:digit:]]+>\\)?$\\)")
       . ,shell-like-action)
      ;; shell-like mode settings
      (,(cons 'or (thattem--mode-list-for-buffer-match
                   thattem-shell-like-modes))
       . ,shell-like-action)
      ;; help mode settings
      (,(cons 'or (thattem--mode-list-for-buffer-match
                   thattem-help-modes))
       . ,help-action)))
  "New value for \\='display-buffer-alist\\='.")

;;; Switch buffer functions

(defun thattem--build-member (equal-func)
  "Return a function which behave like \\='member\\='.
But use EQUAL-FUNC to judge equality.
Like (funcall EQUAL-FUNC elt (car list))."
  (let ((recursion
         (lambda (func elt list)
           (cond ((not list) nil)
                 ((funcall equal-func elt (car list))
                  list)
                 (t
                  (funcall func func elt (cdr list)))))))
    (cond ((eq equal-func 'eq) #'memq)
          ((eq equal-func 'eql) #'memql)
          ((eq equal-func 'equal) #'member)
          (t (lambda (elt list)
               (funcall recursion recursion elt list))))))

(defun thattem--get-type (item list &optional member-func)
  "This function will find ITEM in each sub-list.
And return the first sub-list that contain ITEM.
The LIST should be a list of lists.
If no sub-list contains ITEM, return nil.
Use MEMBER-FUNC to judge if ITEM is in a sub-list.
If MEMBER-FUN is omitted or nil, use \\='member\\=' as default."
  (cond ((not list)
         nil)
        ((if member-func
             (funcall member-func item (car list))
           (member item (car list)))
         (car list))
        (t
         (thattem--get-type item (cdr list) member-func))))

(defun thattem-previous-buffer ()
  "In selected window switch to the nearest previous buffer.
with same major mode type (see \\='thattem-special-mode-list\\=')."
  (interactive)
  (cond
   ((window-minibuffer-p)
    (user-error "Cannot switch buffers in minibuffer window"))
   ((eq (window-dedicated-p) t)
    (user-error "Window is strongly dedicated to its buffer"))
   (t
    (let ((continue t)
          (mode-type
           (thattem--get-type
            major-mode thattem-special-mode-list
            #'provided-mode-derived-p)))
      (while continue
        (let ((new-buffer (switch-to-prev-buffer)))
          (when (not new-buffer)
            (user-error "No previous buffer"))
          (when (or (provided-mode-derived-p
                     major-mode mode-type)
                    (and (not mode-type)
                         (not (thattem--get-type
                               major-mode
                               thattem-special-mode-list
                               #'provided-mode-derived-p))))
            (setq continue nil))))))))

(defun thattem-next-buffer ()
  "In selected window switch to the nearest next buffer.
with same major mode type (see \\='thattem-special-mode-list\\=')."
  (interactive)
  (cond
   ((window-minibuffer-p)
    (user-error "Cannot switch buffers in minibuffer window"))
   ((eq (window-dedicated-p) t)
    (user-error "Window is strongly dedicated to its buffer"))
   (t
    (let* ((continue t)
           (mode-type
            (thattem--get-type
             major-mode thattem-special-mode-list
             #'provided-mode-derived-p)))
      (while continue
        (let ((new-buffer (switch-to-next-buffer)))
          (when (not new-buffer)
            (user-error "No next buffer"))
          (when (or (provided-mode-derived-p
                     major-mode mode-type)
                    (and (not mode-type)
                         (not (thattem--get-type
                               major-mode
                               thattem-special-mode-list
                               #'provided-mode-derived-p))))
            (setq continue nil))))))))

;;; Select window functions

(defun thattem-select-help-window ()
  "Select the first window with major mode in \
\\='thattem-help-modes\\='."
  (interactive)
  (let* ((predicate (lambda (window)
                      (with-selected-window window
                        (derived-mode-p thattem-help-modes))))
         (window (get-window-with-predicate predicate)))
    (when (not window)
      (user-error "No help mode window"))
    (select-window window)))

(defun thattem-select-shell-window ()
  "Select the first window with major mode in \
\\='thattem-shell-like-modes\\='."
  (interactive)
  (let* ((predicate (lambda (window)
                      (with-selected-window window
                        (derived-mode-p thattem-shell-like-modes))))
         (window (get-window-with-predicate predicate)))
    (when (not window)
      (user-error "No shell mode window"))
    (select-window window)))

;; Do what I mean window actions

(defun thattem--get-buffer-with-derived-mode (modes)
  "Return the first buffer with major mode derived from MODES."
  (car
   (funcall
    (thattem--build-member
     (lambda (major-mode-list buffer)
       (with-current-buffer buffer
         (derived-mode-p major-mode-list))))
    modes (buffer-list))))

(defun thattem-help-window-dwim ()
  "If current buffer is Help Buffer, switch to next Help Buffer.
Or if there is another window shows Help Buffer, select that window.
Otherwise show a Help Buffer in a new window.

Help Buffer means buffer with major mode in \
\\='thattem-help-modes\\='."
  (interactive)
  (cond-let ((derived-mode-p thattem-help-modes)
             (thattem-next-buffer))
            ((ignore-errors (thattem-select-help-window)))
            ([buffer (thattem--get-buffer-with-derived-mode
                      thattem-help-modes)]
             (pop-to-buffer buffer))
            (t
             (user-error "No help mode buffer"))))

(defun thattem-shell-window-dwim ()
  "If current buffer is Shell Buffer, switch to next Shell Buffer.
Or if there is another window shows Shell Buffer, select that window.
Otherwise show a Shell Buffer in a new window.

Shell Buffer means buffer with major mode in \
\\='thattem-shell-like-modes\\='."
  (interactive)
  (cond-let ((derived-mode-p thattem-shell-like-modes)
             (thattem-next-buffer))
            ((ignore-errors (thattem-select-shell-window)))
            ([buffer (thattem--get-buffer-with-derived-mode
                      thattem-shell-like-modes)]
             (pop-to-buffer buffer))
            (t
             (user-error "No shell mode buffer"))))

;;; Mode line settings

(thattem-mode-line-define-wrapper-function thattem-previous-buffer)

(thattem-mode-line-define-wrapper-function thattem-next-buffer)

(defun thattem-window-actions-set-keymap ()
  "Modify keymaps in mode line items."
  (define-key thattem-mode-line-buffer-name-keymap
              [mode-line wheel-up]
              #'thattem-mode-line-previous-buffer)
  (define-key thattem-mode-line-buffer-name-keymap
              [mode-line wheel-down]
              #'thattem-mode-line-next-buffer)
  (define-key thattem-mode-line-buffer-name-keymap
              [header-line wheel-up]
              #'thattem-mode-line-previous-buffer)
  (define-key thattem-mode-line-buffer-name-keymap
              [header-line wheel-down]
              #'thattem-mode-line-next-buffer))

(defcustom thattem-shell-header-line-format
  '("%e"
    thattem-mode-line-fire
    thattem-mode-line-buffer-name
    thattem-mode-line-fire-reverse
    thattem-mode-line-major-mode
    thattem-mode-line-file-dir
    thattem-mode-line-end-space-bright)
  "Header line format for shell-like buffer."
  :type '(repeat (choice string symbol))
  :group 'thattem-window-actions)

(defcustom thattem-shell-mode-line-format
  nil
  "Mode line format for shell-like buffer."
  :type '(repeat (choice string symbol))
  :group 'thattem-window-actions)

(defcustom thattem-help-header-line-format
  '("%e"
    thattem-mode-line-header-right-align-bright
    thattem-mode-line-right-slant-reverse
    thattem-mode-line-line-and-column-number)
  "Header line format for help buffer."
  :type '(repeat (choice string symbol))
  :group 'thattem-window-actions)

(defcustom thattem-help-mode-line-format
  '("%e"
    thattem-mode-line-buffer-name
    thattem-mode-line-right-slant
    thattem-mode-line-major-mode
    thattem-mode-line-end-space-bright)
  "Mode line format for help buffer."
  :type '(repeat (choice string symbol))
  :group 'thattem-window-actions)

;;; Hook settings

(defun thattem-add-mode-hook (mode function)
  "A helper function to add hook FUNCTION to the MODE."
  (add-hook (intern (concat (symbol-name mode) "-hook")) function))

(defun thattem-remove-mode-hook (mode function)
  "A helper function to remove hook FUNCTION to the MODE."
  (remove-hook (intern (concat (symbol-name mode) "-hook")) function))

(defun thattem-shell-mode-hook-function ()
  "A function called by the hook of shell-like mode.
like \\='term-mode\\=', \\='shell-mode\\=' and \\='eshell-mode\\='."
  ;; Set header line and mode line
  (setq header-line-format
        thattem-shell-header-line-format)
  (setq mode-line-format
        thattem-shell-mode-line-format)
  ;; Set style
  (setq thattem-mode-line--buffer-style 2)
  ;; Recalculate preserved height
  (when (window-preserved-size)
    (window-preserve-size nil nil t)))

(defun thattem-help-mode-hook-function ()
  "A function called by the hook of help mode."
  ;; Set header line and mode line
  (when (eq header-line-format thattem-default-header-line-format)
    (setq header-line-format thattem-help-header-line-format))
  (when (eq mode-line-format thattem-default-mode-line-format)
    (setq mode-line-format thattem-help-mode-line-format))
  ;; Do not show line number at the left
  (display-line-numbers-mode 0))

(defun thattem-mode-line--advice-around--display-line-numbers (func)
  "Advice to \\='display-line-numbers--turn-on\\=' FUNC.
Make it not turn on display line numbers on help modes."
  (unless (provided-mode-derived-p major-mode thattem-help-modes)
    (funcall func)))

;;; Define minor mode

(define-minor-mode thattem-window-actions-mode
  "Toggle thattem window actions mode."
  :global t

  (when thattem-window-actions-mode
    (dolist (mode thattem-shell-like-modes)
      (thattem-add-mode-hook mode
                             #'thattem-shell-mode-hook-function))
    (dolist (mode thattem-help-modes)
      (thattem-add-mode-hook mode
                             #'thattem-help-mode-hook-function))
    (advice-add
     'display-line-numbers--turn-on :around
     #'thattem-mode-line--advice-around--display-line-numbers))

  (unless thattem-window-actions-mode
    (dolist (mode thattem-shell-like-modes)
      (thattem-remove-mode-hook mode
                                #'thattem-shell-mode-hook-function))
    (dolist (mode thattem-help-modes)
      (thattem-remove-mode-hook mode
                                #'thattem-help-mode-hook-function))
    (advice-remove
     'display-line-numbers--turn-on
     #'thattem-mode-line--advice-around--display-line-numbers))

  (when thattem-window-actions-mode
    (thattem-mode-line-mode)
    ;; Set keymap
    (thattem-window-actions-set-keymap)
    ;; Set window actions (display buffer actions)
    (setq display-buffer-alist thattem-display-buffer-alist-default)
    ;; Run special mode hooks for already exists buffers
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (run-hooks
         (intern (concat (symbol-name major-mode) "-hook")))))))



(provide 'thattem-window-actions)
;;; thattem-window-actions.el ends here
