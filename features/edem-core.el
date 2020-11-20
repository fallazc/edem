;;; edem-core.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;; Author:  <http://github/fallazc>
;; Maintainer:  <fallazc@SBox.SBox.org>
;; Created: November 19, 2020
;; Modified: November 19, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/fallazc/edem-core
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(defvar edem-keymap (make-sparse-keymap))
(defvar edem-keymap-leader "s-SPC")
(defvar edem-keymap-leader-alt (concat "M-" edem-keymap-leader))
(defvar edem-emacs-cmd-fmt (concat "emacsclient -c -s '" server-name "' --eval '(%s)'"))

;; [0] = pid, [1] = command, [2] = output-handler, [3] = finished-handler
(defvar edem-running-processes-info '((nil nil nil nil)))

(defun edem--shell-cmd-process-sentinel (process event)
  "Handles EVENT received by PROCESS."
  (let* ((pid (process-id process))
        (predicate #'(lambda (item) (= pid (elt item 0)))))
    (let ((process-info (seq-find predicate edem-running-processes-info)))
      (when (string-match "finished" event)
        (funcall (elt process-info 3)))
      (remove-if predicate edem-running-processes-info))))

(defun edem--shell-cmd-process-filter (process string)
  "Handles output STRING emitted by PROCESS."
  (let* ((pid (process-id process))
        (process-info (seq-find #'(lambda (item) (= pid (elt item 0)))
                                edem-running-processes-info)))
    (let ((fn (elt process-info 2)))
      (when (functionp fn) (funcall fn string)))))

(defun edem-shell-cmd-run (command &optional output-handler finished-handler)
  "Asynchronously run user's COMMAND.
OUTPUT-HANDLER function that handle the command's output.
FINISHED-HANDLER function that handle the command's output"
  (when (not (seq-find #'(lambda (item) (string-equal command (elt item 1)))
                        edem-running-processes-info))
    (let* ((process (make-process
                     :name command
                     :connection-type 'pipe
                     :command `("sh" "-c" ,command)
                     :buffer (get-buffer-create "*Edem Shell*")
                     :filter #'edem--shell-cmd-process-filter
                     :sentinel #'edem--shell-cmd-process-sentinel)))
      (appendq! edem-running-processes-info
                `((,(process-id process)
                   ,command
                   ,output-handler
                   ,finished-handler))))))

(defmacro define-edem-shell-cmd! (name command &rest args)
  "Helper to define shell command as functions.
NAME - name used to declare the command as function
COMMAND - string command to run
ARGS - keywords :output-handler :finished-handler"
  `(defun ,name ()
     (edem-shell-cmd-run ,command
                         ,(plist-get args :output-handler)
                         ,(plist-get args :finished-handler))))

(provide 'edem-core)
;;; edem-core.el ends here
