;;; edem-bspwm.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;; Author:  <http://github/fallazc>
;; Maintainer:  <fallazc@SBox.SBox.org>
;; Created: November 19, 2020
;; Modified: November 19, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/fallazc/edem-bspwm
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(defvar edem-bspwm-process nil)

(defun edem-bspwm-process-buffer-message (message)
  (with-current-buffer (process-buffer edem-bspwm-process)
    (insert message)))

(defun edem-bspwm-process-sentinel (process event)
  (edem-bspwm-process-buffer-message (format "sentinel event: %s %s" process event))
  (cond
   ((string-match "killed" event)
      nil)))

(defun edem-bspwm-process-filter (process string)
  (edem-bspwm-process-buffer-message (format "%s" string)))

(defun edem-bspwm-process-status ()
  (if edem-bspwm-process
    (process-status edem-bspwm-process)
    nil))

(defun edem-bspwm-process-stop ()
  (when edem-bspwm-process
    (delete-process edem-bspwm-process)))

(defun edem-bspwm-process-start ()
  (when (not (eq 'run (edem-bspwm-process-status)))
    (edem-bspwm-process-stop)
    (setq edem-bspwm-process (make-process
                        :name "bspwm"
                        :connection-type 'pipe
                        :command '("sh" "-c" "bspc subscribe all")
                        :buffer (get-buffer-create "*Window Manager*")
                        :filter #'edem-bspwm-process-filter
                        :sentinel #'edem-bspwm-process-sentinel))))

(provide 'edem-bspwm)
;;; edem-bspwm.el ends here
