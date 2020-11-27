;;; os/edem/features/config.el --- description -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(defvar edem-shell-cmd-prefix "edem-shell-cmd:")
(defvar edem-shell-cmd-space "%%")

(define-edem-shell-cmd! edem-volume-up "pamixer -i 3")
(define-edem-shell-cmd! edem-volume-down "pamixer -d 3")

(define-edem-shell-cmd! launch-rofi "rofi -show run")
(define-edem-shell-cmd! launch-xterm "xterm")

(define-minor-mode edem-minor-mode
  "Toggle Hungry mode....rest of documentation as before..."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Edem Help"
  ;; The minor mode bindings.
  :keymap edem-keymap)


(defun export-keybindings ()
  (with-temp-buffer
    (let ((key-cords) (command))
      (dolist (binding (which-key--get-keymap-bindings edem-keymap t))
        (setq key-cords (replace-regexp-in-string " " " ; " (car binding)))
        (cond
         ((string-match "s-" key-cords) (setq key-cords (replace-regexp-in-string "s-" "super + " key-cords)))
         ((string-match "M-" key-cords) (setq key-cords (replace-regexp-in-string "M-" "meta + " key-cords))))
        (setq command (cdr binding))
        (if (string-match edem-shell-cmd-prefix command)
            (progn
              (setq command (replace-regexp-in-string edem-shell-cmd-space " " command))
              (setq command (replace-regexp-in-string edem-shell-cmd-prefix "" command)))
          (setq command (format edem-emacs-cmd-fmt command)))
        (insert (concat key-cords "\n\t" command "\n\n"))))
    (write-file "~/.config/sxhkd/tmp.txt" nil)))

(provide 'bspwm)
;;; bspwm.el ends here
