;;; edem-xorg-utils.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(require 'xelb)
(require 'xcb-ewmh)

(defvar edem-xorg-error-hook nil
  "Normal hook run when a xorg related error occurs.")

(defvar-local edem--connection nil
  "Connection to Xorg display")

(defun edem-xorg-connect (&optional display)
  "Connect to the x server with DISPLAY.
If the DISPLAY is omitted, the one provided by the environment is used."

  (edem-xorg-disconnect)
  (setq edem--connection (xcb:connect display))

  (if edem--connection
      ;; Initialize ICCCM/EWMH support
      (progn
        (set-process-query-on-exit-flag (slot-value edem--connection 'process)
                                        nil) ;prevent query message on exit
        (xcb:icccm:init edem--connection t)
        (xcb:ewmh:init edem--connection t))
    (run-hook-with-args 'edem-xorg-error-hook "Connection to xorg server failed.")))

(defun edem-xorg-disconnect ()
  "Disconnect from the x server."
  (when edem--connection
    (xcb:flush edem--connection)
    (xcb:disconnect edem--connection))
  (setq edem--connection nil))

(defun edem-xorg-get-window-title (id)
  "Get window title/name using its ID."
  (let ((title)
        (reply (xcb:+request-unchecked+reply edem--connection
                    (make-instance 'xcb:ewmh:get-_NET_WM_NAME :window id))))
    ;nil when destroyed
    (when reply
      (setq title (slot-value reply 'value)))
    title))

(provide 'edem-xorg-utils)
;;; edem-xorg-utils.el ends here
