;;; modeline.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020
;;
;; Author:  <http://github/fallazc>
;; Maintainer:  <fallazc@SBox.SBox.org>
;; Created: November 02, 2020
;; Modified: November 02, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/fallazc/modeline
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
(require 'doom-modeline-segments)

(defvar-local edem-modeline--time "MJJ")
(defun edem-modeline-update-time ()
  "Update "
  (setq edem-modeline--time(format "  %s " (current-time-string))))

;; add my custom hook
;;(defvar bw-after-custom-load-hook nil
;;  "Hook called after the custom file is loaded")

;; but load it after custom has loaded, so it's marked safe
;;(add-hook 'bw-after-custom-load-hook #'my-custom-hook)

;; Load custom file last
;;(setq custom-file (concat dotfiles-dir "custom.el"))
;;(load custom-file 'noerror)

;; load my custom hooks
;;(run-hooks 'bw-after-custom-load-hook)
;;(add-hook 'pdf-view-change-page-hook #'edem-modeline-update-time)

(doom-modeline-def-segment edem-time
"Display "
  (propertize edem-modeline--time
              'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)))

(doom-modeline-def-modeline 'edem
  '(edem-time)
  '(edem-time))

;;;###autoload
(defun doom-modeline-set-edem-modeline ()
  ;;(run-with-timer 1 nil #'edem-modeline-update-time)
  (doom-modeline-set-modeline 'edem))

;; Add hooks
(add-hook 'edem-minor-mode-hook #'doom-modeline-set-edem-modeline)

;;(add-hook 'rcirc-track-minor-mode-hook #'doom-modeline-override-rcirc-modeline)

(provide 'modeline)
;;; modeline.el ends here
