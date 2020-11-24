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

(require 'edem-core)

(defvar edem-bspwm-process nil)

(defun edem-bspwm-config (setting value &optional monitor-sel desktop-sel node-sel)
  "See BSPWM manual for SETTING, VALUE, MONITOR-SEL, DESKTOP-SEL and NODE-SEL."
  (concat "bspc config "
          (and monitor-sel (not (string-empty-p monitor-sel)) "-m ")
          (and desktop-sel (not (string-empty-p desktop-sel)) "-d ")
          (and node-sel (not (string-empty-p node-sel)) "-n ")
          setting " " value))

(defun edem-bspwm-rule-list ()
  "See BSPWM manual."
  "bspc rule -l")

(defun edem-bspwm-rule-add (window-name one-shot &rest args)
  "See BSPWM manual for more info.
WINDOW-NAME string in the format (<class_name>|*)[:(<instance_name>|*)]
ONE-SHOT
ARGS is a list holding an alist where CAR=property name and CDR=value."
  (let ((rules ""))
    (dolist (item args)
      (setq rules (concat rules (concat (car item) "=" (cdr item) " "))))
    (concat "bspc rule -a '" window-name "' " (and one-shot "-o ") rules)))

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
    (setq edem-bspwm-process (edem-shell-cmd-run "bspc subscribe all"
                                                 #'edem-bspwm-process-filter
                                                 #'edem-bspwm-process-sentinel))))

(provide 'edem-bspwm)
;;; edem-bspwm.el ends here
