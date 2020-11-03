;; os/edem/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:
(require 'doom-modeline-core)
(require 'doom-modeline-segments)

(defvar-local edem-modeline--time "")
(defun edem-modeline-update-time ()
  (when (doom-modeline--active)
      (setq edem-modeline--time (format "  %s " (current-time-string)))
      (force-mode-line-update)))

(doom-modeline-def-segment edem-time
  (concat
   (doom-modeline-spc)
   (propertize edem-modeline--time
               'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive))))

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
