;;; os/edem/features/config.el --- description -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(defvar wm-process nil)
(defvar edem-shell-cmd-prefix "edem-shell-cmd:")
(defvar edem-shell-cmd-space "%%")

(define-edem-shell-cmd! edem-volume-up "pamixer -i 3")
(define-edem-shell-cmd! edem-volume-down "pamixer -d 3")

(define-edem-shell-cmd! launch-rofi "rofi -show run")
(define-edem-shell-cmd! launch-xterm "xterm")

(defmacro edem-shell-cmd (command)
  (let ((shell-cmd (replace-regexp-in-string " " edem-shell-cmd-space command)))
    (concat edem-shell-cmd-prefix shell-cmd)))

(map! :map edem-keymap
      "XF86AudioRaiseVolume"    #'edem-volume-up
      "XF86AudioLowerVolume"    #'edem-volume-down
      "XF86AudioMute"           #'edem-volume-toggle-mute
      "XF86AudioPlay"           #'edem-media-play
      "XF86AudioPrev"           #'edem-media-prev
      "XF86AudioNext"           #'edem-media-next
      "XF86MonBrightnessUp"     #'edem-brightness-up
      "XF86MonBrightnessDown"   #'edem-brightness-down

      (:prefix edem-keymap-leader
       ;;; <leader> TAB --- workspace
       (:when (featurep! +workspace)
        (:prefix ("TAB" . "workspace")
         :desc "Switch workspace"          "."   #'edem-workspace-switch-to
         :desc "Switch to last workspace"  "`"   #'edem-workspace-other
         :desc "New workspace"             "n"   #'edem-workspace-new
         :desc "Load workspace from file"  "l"   #'edem-workspace-load
         :desc "Save workspace to file"    "s"   #'edem-workspace-save
         :desc "Delete this workspace"     "d"   #'edem-workspace-delete
         :desc "Rename workspace"          "r"   #'edem-workspace-rename
         :desc "Restore last session"      "R"   #'edem-workspace-restore-last-session
         :desc "Next workspace"            "]"   #'edem-workspace-switch-right
         :desc "Previous workspace"        "["   #'edem-workspace-switch-left
         :desc "Switch to 1st workspace"   "1"   #'edem-workspace-switch-to-0
         :desc "Switch to 2nd workspace"   "2"   #'edem-workspace-switch-to-1
         :desc "Switch to 3rd workspace"   "3"   #'edem-workspace-switch-to-2
         :desc "Switch to 4th workspace"   "4"   #'edem-workspace-switch-to-3
         :desc "Switch to 5th workspace"   "5"   #'edem-workspace-switch-to-4
         :desc "Switch to 6th workspace"   "6"   #'edem-workspace-switch-to-5
         :desc "Switch to 7th workspace"   "7"   #'edem-workspace-switch-to-6
         :desc "Switch to 8th workspace"   "8"   #'edem-workspace-switch-to-7
         :desc "Switch to 9th workspace"   "9"   #'edem-workspace-switch-to-8
         :desc "Switch to final workspace" "0"   #'edem-workspace-switch-to-final))

       ;;; <leader> b --- buffer
       (:prefix ("b" . "buffer")
        (:when (featurep! +workspace)
         :desc "Switch workspace buffer" "b" #'edem-switch-to-workspace-buffer
         :desc "Switch buffer"           "B" #'edem-switch-to-buffer)
        (:unless (featurep! +workspace)
         :desc "Switch buffer"           "b" #'edem-switch-to-buffer)
        :desc "Close buffer"                "d"   #'edem-close-current-buffer
        :desc "Kill buffer"                 "k"   #'edem-kill-current-buffer
        :desc "Kill all buffers"            "K"   #'edem-kill-all-buffers
        :desc "Switch to last buffer"       "l"   #'edem-switch-to-windows-last-buffer
        :desc "Set bookmark"                "m"   #'edem-bookmark-set
        :desc "Delete bookmark"             "M"   #'edem-bookmark-delete
        :desc "Next buffer"                 "n"   #'edem-next-buffer
        :desc "New empty buffer"            "N"   #'edem-buffer-new
        :desc "Kill other buffers"          "O"   #'edem-kill-other-buffers
        :desc "Previous buffer"             "p"   #'edem-previous-buffer
        :desc "Bury buffer"                 "z"   #'edem-bury-buffer
        :desc "Kill buried buffers"         "Z"   #'edem-kill-buried-buffers)

       ;;; <leader> q --- window
       (:prefix ("w" . "window")
        :desc "Focus window left"       "h"     #'edem-window-left
        :desc "Focus window down"       "j"     #'edem-window-down
        :desc "Focus window up"         "k"     #'edem-window-up
        :desc "Focus window right"      "l"     #'edem-window-right
        :desc "Move window left"        "H"     #'edem-window-move-left
        :desc "Move window down"        "J"     #'edem-window-move-down
        :desc "Move window up"          "K"     #'edem-window-move-up
        :desc "Move window right"       "L"     #'edem-window-move-right
        :desc "Swap window left"        "C-h"   #'edem-window-swap-left
        :desc "Swap window down"        "C-j"   #'edem-window-swap-down
        :desc "Swap window up"          "C-k"   #'edem-window-swap-up
        :desc "Swap window right"       "C-l"   #'edem-window-swap-right
        :desc "Create window"           "c"     #'edem-window-create
        :desc "Delete window"           "d"     #'edem-window-delete
        (:prefix "m"
         :desc "Maximize window"                 "m"       #'edem-window-maximize-buffer
         :desc "Maximize window vertically"      "v"       #'edem-window-maximize-vertically
         :desc "Maximize window horizontally"    "s"       #'edem-window-maximize-horizontally))

       ;;; <leader> q --- quit/session
       (:prefix ("s" . "quit/session")
        :desc "Clear current frame"          "F" #'edem-kill-all-buffers
        :desc "Quit Edem"                    "q" #'edem-save-buffers-kill-terminal
        :desc "Quit Edem without saving"     "Q" #'edem-quit-all-with-error-code
        :desc "Restore last session"         "l" #'edem-quickload-session
        :desc "Save session to file"         "s" #'edem-save-session
        :desc "Restore session from file"    "L" #'edem-load-session
        :desc "Restart & restore Edem"       "r" #'edem-restart-and-restore
        :desc "Restart Edem"                 "R" #'edem-restart)

       ;;; <leader> r --- remote
       (:prefix ("r" . "run")
        :desc "Program launcher"        "r"      #'launch-rofi
        :desc "Terminal"                "RET"    #'launch-xterm)))

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

(defun wm-process-buffer-message (message)
  (with-current-buffer (process-buffer wm-process)
    (insert message)))

(defun wm-process-sentinel (process event)
  (wm-process-buffer-message (format "sentinel event: %s %s" process event))
  (cond
   ((string-match "killed" event)
      nil)))

(defun wm-process-filter (process string)
  (wm-process-buffer-message (format "%s" string)))

(defun wm-process-status ()
  (if wm-process
    (process-status wm-process)
    nil))

(defun wm-process-stop ()
  (interactive)
  (when wm-process
    (delete-process wm-process)))

(defun wm-process-start ()
  (interactive)
  (when (not (eq 'run (wm-process-status)))
    (wm-process-stop)
    (setq wm-process (make-process
                        :name "bspwm"
                        :connection-type 'pipe
                        :command '("sh" "-c" "bspc subscribe all")
                        :buffer (get-buffer-create "*Window Manager*")
                        :filter #'wm-process-filter
                        :sentinel #'wm-process-sentinel))))

(provide 'bspwm)
;;; bspwm.el ends here
