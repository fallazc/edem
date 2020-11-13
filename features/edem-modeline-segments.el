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
    (let ((cpu-temp))
      (with-temp-buffer
        (insert-file-contents "/sys/class/hwmon/hwmon0/temp1_input")
        (setq cpu-temp (/ (string-to-number (buffer-string)) 1000)))
      (setq edem-modeline--cpu-temp (format " %s °" cpu-temp)))
    (force-mode-line-update)))

(doom-modeline-def-segment edem-cpu-temp
  (concat
   (doom-modeline-spc)
   (propertize edem-modeline--cpu-temp
               'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive))))

(defvar-local edem-modeline--memory "")

(defsubst edem--parse-meminfo-line (linenum)
  (forward-line (- linenum (line-number-at-pos)))
  (let ((line (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))))
    (let ((prop-start 0)
          (prop-end (string-match ":" line))
          (value-start (string-match "[0-9]" line))
          (value-end (- (length line) 3)))
      (list (intern (substring line prop-start prop-end))
            (substring line value-start value-end)))))

(defsubst edem--get-meminfo-plist ()
  (when (doom-modeline--active)
    (let ((mem-usage-plist)
          (prop-lines (vector 1 2 4 5 15 16 21 24)))
      (with-temp-buffer
        (insert-file-contents "/proc/meminfo")
        (seq-doseq (i prop-lines)
          (appendq! mem-usage-plist (edem--parse-meminfo-line i))))
      mem-usage-plist)))

(defun edem-modeline-update-memory ()
  (when (doom-modeline--active)
    (let ((mem-usage-plist (edem-get-meminfo-plist))))
    (force-mode-line-update)))

(doom-modeline-def-segment edem-memory
  (concat
   (doom-modeline-spc)
   (propertize edem-modeline--memory
               'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive))))

(defvar-local edem-modeline--cpu-usages "")
(defvar-local edem-modeline--cpu-old-stat nil)
(defvar-local edem-modeline--cpu-new-stat nil)
(defvar edem-read-n-cpus 4
  "Number of CPU in the system.")
(defvar edem-modeline-cpu-usages-upade-delay 2
  "Number of seconds between refreshing cpu usages.")
(defvar edem-modeline--cpu-usages-upade-timer nil
  "Timer used to schedule the cpu usages update function.")

(defun edem--read-cpu-usages ()
  (let ((cpu-usages (make-vector (* edem-read-n-cpus 4) 0)))
    (with-temp-buffer
      (insert-file-contents "/proc/stat")
      (forward-line 1)
      (dotimes (i edem-read-n-cpus)
        (let ((stat-columns (split-string (buffer-substring-no-properties
                                           (line-beginning-position)
                                           (line-end-position))
                                          " "))
              (offset (* i edem-read-n-cpus)))
          ;;1 = user; 2 = nice; 3 = system; 4 = idle
          (dotimes (j 4)
            (let ((new-value (string-to-number (elt stat-columns (+ j 1)))))
            (aset cpu-usages (+ offset j) new-value)))
        (forward-line 1)))
    cpu-usages)))

(defsubst edem--compute-cpu-usages ()
  "Compute the cpu usage using values from /proc/stat."
  (let ((sum-old-stat (make-vector edem-read-n-cpus 0))
        (sum-new-stat (make-vector edem-read-n-cpus 0)))
    (dotimes (i edem-read-n-cpus)
      (dotimes (j 4)
        (let ((offset (+ (* i edem-read-n-cpus) j)))
          (let ((old-tmp (elt edem-modeline--cpu-old-stat offset))
                (new-tmp (elt edem-modeline--cpu-new-stat offset)))
            (aset sum-old-stat i (+ (elt sum-old-stat i) old-tmp))
            (aset sum-new-stat i (+ (elt sum-new-stat i) new-tmp))))))
    (let ((cpu-usages (make-vector edem-read-n-cpus 0.0)))
      (dotimes (i edem-read-n-cpus)
        (let ((delta-usage (- (elt sum-new-stat i) (elt sum-old-stat i)))
              (idle-index (+ (* i 4) 3)))
          (aset cpu-usages i (* 100 (/ (+ (- delta-usage
                                             (elt edem-modeline--cpu-new-stat idle-index))
                                          (elt edem-modeline--cpu-old-stat idle-index))
                                       (float delta-usage))))))
      cpu-usages)))

(defun edem-modeline-update-cpu-usage (&optional new-usages)
  "You should never call this function manually.
NEW-USAGES will be true when called from the timer function."
  (when (doom-modeline--active)
    (if new-usages
        (progn
          (setq edem-modeline--cpu-new-stat (edem--read-cpu-usages))
          (let ((cpu-usages (edem--compute-cpu-usages)))
            (setq edem-modeline--cpu-usages (format "%s" cpu-usages)))
          (force-mode-line-update))
      (progn
        (setq edem-modeline--cpu-old-stat (edem--read-cpu-usages))
        (print edem-modeline--cpu-old-stat)
        (setq edem-modeline--cpu-usages-upade-timer
              (run-with-timer edem-modeline-cpu-usages-upade-delay nil #'edem-modeline-update-cpu-usage t))))))

(doom-modeline-def-segment edem-cpu-usage
  (concat
   (doom-modeline-spc)
   (propertize edem-modeline--cpu-usages
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
      (setq edem-modeline--bluetooth "  bluetooth" )
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
  "Maybe use DOOM's built-in battery?"
  (when (doom-modeline--active)))

(doom-modeline-def-segment edem-battery
  (concat
   (doom-modeline-spc)
   (propertize edem-modeline--battery
               'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive))))

(provide 'edem-modeline-segments)
;;; edem-modeline-segments.el ends here
