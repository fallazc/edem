;;; edem-modeline.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;; Author:  <http://github/fallazc>
;; Maintainer:  <fallazc@SBox.SBox.org>
;; Created: November 05, 2020
;; Modified: November 05, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/fallazc/edem-modeline
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(require 'doom-modeline-core)
(require 'edem-modeline-segments)

(doom-modeline-def-modeline 'edem
  '(edem-time) '(edem-time))

(defun doom-modeline-set-edem-modeline ()
  (doom-modeline-set-modeline 'edem))

(defun edem-modeline-show ()
  (doom-modeline-set-edem-modeline)
  (run-with-timer 1 1 #'edem-modeline-update-time))

(defun edem-modeline-hide ()
  (doom-modeline-set-main-modeline))

(define-minor-mode edem-modeline-mode
  "Toggle Hungry mode....rest of documentation as before..."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Edem Help"
  (if edem-modeline-mode
      (edem-modeline-show)
    (edem-modeline-hide)))

(provide 'edem-modeline)
;;; edem-modeline.el ends here
