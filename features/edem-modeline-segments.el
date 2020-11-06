;;; edem-modeline-segments.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;; Author:  <http://github/fallazc>
;; Maintainer:  <fallazc@SBox.SBox.org>
;; Created: November 05, 2020
;; Modified: November 05, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/fallazc/edem-modeline-segments
;; Package-Requires: ((emacs 28.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

;;(require 'doom-modeline-segments)

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

(defvar-local edem-modeline--workspaces "")
(defun edem-modeline-update-workspaces ()
  (when (doom-modeline--active)
      (setq edem-modeline--workspaces "  workspaces" )
      (force-mode-line-update)))

(doom-modeline-def-segment edem-workspaces
  (concat
   (doom-modeline-spc)
   (propertize edem-modeline--workspaces
               'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive))))

(defvar-local edem-modeline--cpu-temp "")
(defun edem-modeline-update-cpu-temp ()
  (when (doom-modeline--active)
    (with-temp-buffer
      (insert-file-contents "/sys/class/hwmon/hwmon0/temp1_input")
      (let ((cpu-temp (/ (string-to-number (buffer-string)) 1000)))
        (setq edem-modeline--cpu-temp (format " %d °" cpu-temp))))
    (force-mode-line-update)))

(doom-modeline-def-segment edem-cpu-temp
  (concat
   (doom-modeline-spc)
   (propertize edem-modeline--cpu-temp
               'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive))))

(defvar-local edem-modeline--cpu-load "")
(defun edem-modeline-update-cpu-load ()
  (when (doom-modeline--active)
      (setq edem-modeline--cpu-load "  cpu-load" )
      (force-mode-line-update)))

(doom-modeline-def-segment edem-cpu-load
  (concat
   (doom-modeline-spc)
   (propertize edem-modeline--cpu-load
               'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive))))

(defvar-local edem-modeline--audio "")
(defun edem-modeline-update-audio ()
  (when (doom-modeline--active)
      (setq edem-modeline--audio "  audio" )
      (force-mode-line-update)))

(doom-modeline-def-segment edem-audio
  (concat
   (doom-modeline-spc)
   (propertize edem-modeline--audio
               'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive))))

(defvar-local edem-modeline--brightness "")
(defun edem-modeline-update-birghtness ()
  (when (doom-modeline--active)
      (setq edem-modeline--brightness "  brightness" )
      (force-mode-line-update)))

(doom-modeline-def-segment edem-brightness
  (concat
   (doom-modeline-spc)
   (propertize edem-modeline--brightness
               'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive))))

(defvar-local edem-modeline--bluetooth "")
(defun edem-modeline-update-bluetooth ()
  (when (doom-modeline--active)
      (setq edem-modeline--wifi "  bluetooth" )
      (force-mode-line-update)))

(doom-modeline-def-segment edem-bluetooth
  (concat
   (doom-modeline-spc)
   (propertize edem-modeline--bluetooth
               'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive))))

(defvar-local edem-modeline--wifi "")
(defun edem-modeline-update-wifi ()
  (when (doom-modeline--active)
      (setq edem-modeline--wifi "  wifi" )
      (force-mode-line-update)))

(doom-modeline-def-segment edem-wifi
  (concat
   (doom-modeline-spc)
   (propertize edem-modeline--wifi
               'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive))))

(defvar-local edem-modeline--battery "")
(defun edem-modeline-update-battery ()
  (when (doom-modeline--active)
      (setq edem-modeline--battery "  battery" )
      (force-mode-line-update)))

(doom-modeline-def-segment edem-battery
  (concat
   (doom-modeline-spc)
   (propertize edem-modeline--battery
               'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive))))

(provide 'edem-modeline-segments)
;;; edem-modeline-segments.el ends here
